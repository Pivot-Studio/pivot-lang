use crate::ast::pltype::PLType;
use crate::ast::pltype::STType;

use rustc_hash::FxHashMap;


use crate::ast::pltype::PriType;

use crate::ast::diag::ErrorCode;


use super::*;


use super::Literal;



use super::MatchArmCondition;

use crate::ast::ctx::Ctx;

use super::MatchNode;

/// 表示一个字段的可能模式
pub(crate) struct FieldPattern {
    pub field_name: String,
    pub patterns: Vec<String>,
}

/// 字段占位符标记，使用不可能出现在用户代码中的特殊标记
pub(crate) const FIELD_PLACEHOLDER: &str = "^^PLACEHOLDER__";

/// 通用的quick fix生成函数
///
/// 根据给定的模式列表和缩进生成用于快速修复的文本
///
/// # 参数
/// * `patterns` - 需要添加的模式列表
/// * `indent` - 每个模式的缩进
///
/// # 返回值
/// 格式化好的可作为quick fix使用的文本
pub(crate) fn generate_quick_fix_text(patterns: &[String], indent: &str) -> String {
    let mut result = String::new();
    for pattern in patterns {
        result.push_str(&format!("\n{}{}", indent, pattern));
    }
    result
}

/// 生成字段模式的所有可能组合
///
/// 这个函数会计算所有字段模式的笛卡尔积，生成所有可能的组合
///
/// # 参数
/// * `field_patterns` - 字段模式列表
/// * `template` - 模式模板，用于格式化
/// * `indent` - 缩进
///
/// # 返回值
/// 所有可能的模式组合
pub(crate) fn generate_combined_patterns(
    field_patterns: &[FieldPattern],
    template: &str,
    indent: &str
) -> String {
    if field_patterns.is_empty() {
        return String::new();
    }
    
    // 递归生成所有可能的组合
    let combinations = generate_pattern_combinations(field_patterns, 0, Vec::new());
    
    // 生成最终的QuickFix文本
    let mut result = String::new();
    for combo in combinations {
        // 创建字段替换映射
        let replacements: FxHashMap<String, String> = combo.into_iter()
            .map(|(field, pattern)| (format!("{}:{}", field, FIELD_PLACEHOLDER), pattern))
            .collect();
        
        // 应用替换到模板
        let mut pattern_str = template.to_string();
        for (placeholder, replacement) in replacements {
            pattern_str = pattern_str.replace(&placeholder, &replacement);
        }
        
        // 处理任何未被替换的占位符 - 将它们替换为通配符
        pattern_str = pattern_str.replace(&format!(":{}", FIELD_PLACEHOLDER), ":_");
        
        // 添加到结果
        result.push_str(&format!("\n{}{} => {{}}", indent, pattern_str));
    }
    
    result
}

/// 递归生成所有可能的模式组合
///
/// # 参数
/// * `field_patterns` - 字段及其可能模式列表
/// * `index` - 当前处理的字段索引
/// * `current` - 当前已选择的组合
///
/// # 返回值
/// 所有可能组合的列表
fn generate_pattern_combinations(
    field_patterns: &[FieldPattern],
    index: usize,
    current: Vec<(String, String)>
) -> Vec<Vec<(String, String)>> {
    if index >= field_patterns.len() {
        // 基本情况：已经处理完所有字段
        return vec![current];
    }
    
    let field = &field_patterns[index];
    let mut result = Vec::new();
    
    // 为当前字段的每个可能模式创建一个组合
    for pattern in &field.patterns {
        let mut new_combo = current.clone();
        new_combo.push((field.field_name.clone(), pattern.clone()));
        
        // 递归处理下一个字段
        let sub_combos = generate_pattern_combinations(field_patterns, index + 1, new_combo);
        result.extend(sub_combos);
    }
    
    result
}

impl MatchNode {
    /// 检查match表达式是否穷尽（覆盖了所有可能情况）
    ///
    /// 穷尽性检查确保match表达式处理了所有可能的输入值。
    /// 对于不同类型有不同的穷尽性要求：
    /// - 布尔类型需要明确匹配true和false
    /// - 枚举类型需要匹配所有成员
    /// - 数值类型（整数、浮点数）需要通配符或变量绑定
    ///
    /// # 参数
    /// * `ctx` - 编译上下文
    /// * `ty` - 被匹配表达式的类型
    pub(crate) fn check_exhaustiveness<'a, 'b>(&self, ctx: &'b mut Ctx<'a>, ty: &Arc<RefCell<PLType>>) {
        // 如果已有通配符匹配，则一定是穷尽的
        if self.has_wildcard_pattern() {
            return;
        }

        // 确定匹配表达式的结束位置，用于添加quick fix
        let match_end_pos = if let Some((_, last_arm)) = self.arms.last() {
            last_arm.range().end
        } else {
            // 如果没有匹配分支，使用整个匹配表达式的结束位置
            self.range.end
        };

        // 计算适当的缩进
        let indent = self.calculate_indent(ctx);

        match &*ty.borrow() {
            PLType::Primitive(p) => self.check_primitive_exhaustiveness(ctx, p, match_end_pos, &indent),
            PLType::Union(u) => self.check_union_exhaustiveness(ctx, u, ty, match_end_pos, &indent),
            PLType::Struct(s) if s.is_tuple => self.check_tuple_exhaustiveness(ctx, s, ty, match_end_pos, &indent),
            PLType::Struct(s) => self.check_struct_exhaustiveness(ctx, s, ty, match_end_pos, &indent),
            _ => {
                // 非枚举类型，如果没有通配符匹配，则发出警告
                if !self.has_wildcard_pattern() {
                    self.report_non_exhaustive_pattern(
                        ctx,
                        "此匹配不穷尽，需要添加 `_` 捕获所有其他情况".to_string(),
                        vec!["_ => {}".to_string()],
                        match_end_pos,
                        &indent,
                        None,
                        None,
                    );
                }
            }
        }
    }

    /// 计算基于现有模式的正确缩进
    /// 
    /// 基于第一个匹配分支的缩进来计算后续模式的缩进，
    /// 以保持代码格式一致性
    ///
    /// # 参数
    /// * `ctx` - 编译上下文
    ///
    /// # 返回值
    /// 计算出的缩进字符串
    pub(crate) fn calculate_indent(&self, ctx: &mut Ctx) -> String {
        // 获取第一个匹配分支的缩进
        if let Some((first_arm, _)) = self.arms.first() {
            // 获取该arm的起始位置
            let start_pos = first_arm.range().start;
        
            // 基于列号计算缩进（减1是因为列号从1开始）
            return " ".repeat(start_pos.column - 1);
        }
    
        // 默认缩进
        "    ".to_string()
    }

    /// 检查是否包含通配符模式
    /// 
    /// 通配符模式（如 `_`）可以匹配任何值，
    /// 如果存在通配符，则匹配表达式一定是穷尽的
    ///
    /// # 返回值
    /// 如果有通配符模式返回true，否则返回false
    pub(crate) fn has_wildcard_pattern(&self) -> bool {
        self.arms.iter().any(|(cond, _)| self.is_wildcard_pattern(cond))
    }

    /// 检查是否包含变量绑定模式
    /// 
    /// 变量绑定（如 `x`）可以匹配任何值，
    /// 对于一些无限值类型（如整数、浮点数），变量绑定也能确保穷尽
    ///
    /// # 返回值
    /// 如果有变量绑定模式返回true，否则返回false
    pub(crate) fn has_variable_binding(&self) -> bool {
        self.arms.iter().any(|(cond, _)| self.is_variable_binding(cond))
    }

    /// 递归检查一个模式是否为变量绑定（包括嵌套模式）
    ///
    /// # 参数
    /// * `cond` - 要检查的匹配条件
    ///
    /// # 返回值
    /// 如果模式是变量绑定返回true，否则返回false
    pub(crate) fn is_variable_binding(&self, cond: &MatchArmCondition) -> bool {
        match cond {
            MatchArmCondition::Discard(_) => false,
            MatchArmCondition::Var(_) => true, // 变量绑定模式
            MatchArmCondition::Literal(_) => false,
            MatchArmCondition::TypedVar(_, inner) => self.is_variable_binding(inner),
            MatchArmCondition::TypedDeconstruct(_, fields) => {
                fields.iter().any(|(_, c)| self.is_variable_binding(c))
            },
            MatchArmCondition::Deconstruct(fields,_) => {
                fields.iter().any(|(_, c)| self.is_variable_binding(c))
            },
            MatchArmCondition::Tuple(fields, _) => {
                fields.iter().any(|c| self.is_variable_binding(c))
            }
        }
    }

    /// 递归检查一个模式是否为通配符（包括嵌套模式）
    ///
    /// # 参数
    /// * `cond` - 要检查的匹配条件
    ///
    /// # 返回值
    /// 如果模式是通配符返回true，否则返回false
    pub(crate) fn is_wildcard_pattern(&self, cond: &MatchArmCondition) -> bool {
        match cond {
            MatchArmCondition::Discard(_) => true,
            MatchArmCondition::Var(_) => false, // 变量绑定不应被视作通配符匹配
            MatchArmCondition::Literal(_) => false,
            MatchArmCondition::TypedVar(_, inner) => self.is_wildcard_pattern(inner),
            MatchArmCondition::TypedDeconstruct(_, fields) => {
                fields.iter().all(|(_, c)| self.is_wildcard_pattern(c))
            },
            MatchArmCondition::Deconstruct(fields,_) => {
                fields.iter().all(|(_, c)| self.is_wildcard_pattern(c))
            },
            MatchArmCondition::Tuple(fields, _) => {
                fields.iter().all(|c| self.is_wildcard_pattern(c))
            }
        }
    }

    /// 通用的非穷尽匹配错误报告方法
    /// 
    /// # 参数
    /// * `ctx` - 编译上下文
    /// * `message` - 主要错误消息
    /// * `patterns` - 要添加的匹配模式列表（用于quick fix）
    /// * `match_end_pos` - 匹配表达式结束位置，用于插入quick fix
    /// * `indent` - 缩进字符串
    /// * `help_message` - 可选的帮助消息
    /// * `additional_labels` - 可选的额外标签列表，每个元素包含位置和消息
    pub(crate) fn report_non_exhaustive_pattern<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        message: String,
        patterns: Vec<String>, // 推荐添加的模式
        match_end_pos: Pos,
        indent: &str,
        help_message: Option<String>,
        additional_labels: Option<Vec<(Range, String)>>,
    ) {
        // 创建诊断
        let mut diag = self.range
            .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
        let mut diag = diag
            .add_label(
                self.range,
                ctx.get_file(),
                format_label!(
                    "{}",
                    message
                ),
            );
    
        // 添加额外的标签
        if let Some(labels) = additional_labels {
            for (range, message) in labels {
                diag.add_label(
                    range,
                    ctx.get_file(),
                    format_label!("{}", message)
                );
            }
        }
    
        // 添加help消息
        if let Some(help) = help_message {
            diag.add_help(&help);
        }
    
        // 添加quick fix
        let quick_fix_text = generate_quick_fix_text(&patterns, indent);
        diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
    
        // 添加到上下文
        diag.add_to_ctx(ctx);
    }

    /// 检查简单类型（如布尔、字符）的穷尽性
    ///
    /// # 参数
    /// * `ctx` - 编译上下文
    /// * `p` - 基本类型
    /// * `match_end_pos` - 匹配表达式结束位置
    /// * `indent` - 缩进字符串
    pub(crate) fn check_primitive_exhaustiveness<'a, 'b>(&self, ctx: &'b mut Ctx<'a>, p: &PriType, match_end_pos: Pos, indent: &str) {
        match p {
            PriType::BOOL => {
                // 布尔类型需要检查true和false是否都被覆盖
                let has_true = self.arms.iter().any(|(cond, _)| self.matches_literal_bool(cond, true));
                let has_false = self.arms.iter().any(|(cond, _)| self.matches_literal_bool(cond, false));

                if !has_true || !has_false {
                    let mut patterns = Vec::new();
                    if !has_true {
                        patterns.push("true => {}".to_string());
                    }
                    if !has_false {
                        patterns.push("false => {}".to_string());
                    }
                
                    self.report_non_exhaustive_pattern(
                        ctx,
                        "此匹配不穷尽，布尔类型需要匹配 'true' 和 'false'".to_string(),
                        patterns,
                        match_end_pos,
                        indent,
                        None,
                        None,
                    );
                }
            }
            PriType::I8 | PriType::I16 | PriType::I32 | PriType::I64 |
            PriType::U8 | PriType::U16 | PriType::U32 | PriType::U64 => {
                // 整数类型需要通配符或变量绑定
                if !self.has_wildcard_pattern() && !self.has_variable_binding() {
                    let type_name = p.get_name();
                    let message = format!("此匹配不穷尽，整数类型 {} 有无限可能值，需要通配符或变量绑定", type_name);
                    let help_message = "考虑添加 `_ => {}` 或者 `x => {}` 来匹配所有其他整数值".to_string();
                
                    self.report_non_exhaustive_pattern(
                        ctx,
                        message,
                        vec!["_ => {}".to_string()],
                        match_end_pos,
                        indent,
                        Some(help_message),
                        None,
                    );
                }
            }
            PriType::F32 | PriType::F64 => {
                // 浮点类型需要通配符或变量绑定
                if !self.has_wildcard_pattern() && !self.has_variable_binding() {
                    let type_name = p.get_name();
                    let message = format!("此匹配不穷尽，浮点类型 {} 有无限可能值，需要通配符或变量绑定", type_name);
                    let help_message = "考虑添加 `_ => {}` 或者 `x => {}` 来匹配所有其他浮点值".to_string();
                
                    self.report_non_exhaustive_pattern(
                        ctx,
                        message,
                        vec!["_ => {}".to_string()],
                        match_end_pos,
                        indent,
                        Some(help_message),
                        None,
                    );
                }
            }
            PriType::CHAR => {
                // 字符类型需要通配符或变量绑定
                if !self.has_wildcard_pattern() && !self.has_variable_binding() {
                    let message = "此匹配不穷尽，字符类型有无限可能值，需要通配符或变量绑定".to_string();
                    let help_message = "考虑添加 `_ => {}` 或者 `c => {}` 来匹配所有其他字符".to_string();
                
                    self.report_non_exhaustive_pattern(
                        ctx,
                        message,
                        vec!["_ => {}".to_string()],
                        match_end_pos,
                        indent,
                        Some(help_message),
                        None,
                    );
                }
            }
            _ => {
                // 对于其他原始类型，如果既没有通配符，也没有变量绑定，就需要警告
                if !self.has_wildcard_pattern() && !self.has_variable_binding() {
                    let message = format!(
                        "此匹配不穷尽，类型 {} 需要添加 `_` 或变量绑定捕获所有其他值", 
                        p.get_name()
                    );
                    let help_message = "添加通配符 `_` 或变量绑定来匹配所有可能值".to_string();
                
                    self.report_non_exhaustive_pattern(
                        ctx,
                        message,
                        vec!["_ => {}".to_string()],
                        match_end_pos,
                        indent,
                        Some(help_message),
                        None,
                    );
                }
            }
        }
    }

    /// 递归检查是否匹配特定布尔值
    ///
    /// # 参数
    /// * `cond` - 要检查的匹配条件
    /// * `value` - 要匹配的布尔值
    ///
    /// # 返回值
    /// 如果条件匹配指定布尔值返回true，否则返回false
    pub(crate) fn matches_literal_bool(&self, cond: &MatchArmCondition, value: bool) -> bool {
        match cond {
            MatchArmCondition::Discard(_) => true, // 通配符匹配任何值
            MatchArmCondition::Var(_) => true, // 变量绑定匹配任何值
            MatchArmCondition::Literal(Literal::Bool(b)) => b.value == value,
            MatchArmCondition::TypedVar(_, inner) => self.matches_literal_bool(inner, value),
            MatchArmCondition::TypedDeconstruct(_, _) => false, // 结构拆解不匹配布尔值
            MatchArmCondition::Deconstruct(_,_) => false, // 结构拆解不匹配布尔值
            MatchArmCondition::Tuple(_, _) => false, // 元组不匹配布尔值
            _ => false,
        }
    }

    /// 检查联合类型的穷尽性
    ///
    /// # 参数
    /// * `ctx` - 编译上下文
    /// * `u` - 联合类型
    /// * `ty` - 完整类型
    /// * `match_end_pos` - 匹配表达式结束位置
    /// * `indent` - 缩进字符串
    pub(crate) fn check_union_exhaustiveness<'a, 'b>(
        &self, 
        ctx: &'b mut Ctx<'a>, 
        u: &crate::ast::pltype::UnionType,
        ty: &Arc<RefCell<PLType>>,
        match_end_pos: Pos,
        indent: &str
    ) {
        let sum_types = u.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
    
        // 收集所有已匹配的类型和完整匹配
        let mut matched_exact_types = Vec::new();  // 精确匹配的类型路径
        let mut matched_full_types = Vec::new();   // 完整匹配的联合类型（包括其所有子类型）
    
        // 收集每种匹配模式的源码位置，用于错误提示
        let mut pattern_locations = FxHashMap::<String, Range>::default();
    
        // 检查直接匹配的类型
        for (cond, _) in &self.arms {
            self.collect_matched_union_types_with_loc(ctx, cond, &mut matched_exact_types, &mut matched_full_types, &mut pattern_locations);
        }
    
        // 检查是否所有联合类型成员都被匹配
        let mut missing_types = Vec::new();  // 完全未匹配的类型
        let mut partially_matched_types = Vec::new();  // 部分匹配的类型及其未匹配子类型
    
        for sum_type in &sum_types {
            let type_name = sum_type.borrow().get_name().to_string();
        
            // 如果整个类型被完整匹配了，则跳过其子类型的检查
            if matched_full_types.contains(&type_name) {
                continue;
            }
        
            // 检查是否直接匹配了这个类型
            if matched_exact_types.iter().any(|t| *t == type_name) {
                continue;
            }
        
            // 对于嵌套的联合类型，检查其子类型
            if let PLType::Union(inner_union) = &*sum_type.borrow() {
                let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                let mut all_inner_matched = true;
                let mut unmatched_inner_types = Vec::new();
            
                // 检查这个联合类型的每个子类型是否被匹配
                for inner_type in inner_types {
                    let inner_type_name = inner_type.borrow().get_name().to_string();
                    let full_path = format!("{}({})", type_name, inner_type_name);
                
                    // 检查是否有直接匹配此嵌套类型的模式
                    if !matched_exact_types.iter().any(|path| *path == full_path) {
                        all_inner_matched = false;
                        unmatched_inner_types.push(inner_type_name);
                    }
                }
            
                // 如果内部所有类型都被匹配，则认为外部类型也被匹配
                if all_inner_matched {
                    continue;
                }
            
                // 如果有部分子类型被匹配，则记录为部分匹配
                if !unmatched_inner_types.is_empty() && unmatched_inner_types.len() < inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default())).len() {
                    partially_matched_types.push((type_name, unmatched_inner_types));
                    continue;
                }
            }
        
            // 如果所有检查都未通过，则记录这个未匹配的类型
            missing_types.push(type_name);
        }
    
        // 如果有未匹配的类型且没有通配符，报错
        if (!missing_types.is_empty() || !partially_matched_types.is_empty()) && !self.has_wildcard_pattern() {
            let mut error_msg = String::new();
        
            if !missing_types.is_empty() {
                error_msg.push_str(&format!("未处理这些类型: {}", missing_types.join(", ")));
            }
        
            if !partially_matched_types.is_empty() {
                if !error_msg.is_empty() {
                    error_msg.push_str("; ");
                }
            
                for (parent_type, missing_subtypes) in &partially_matched_types {
                    error_msg.push_str(&format!("类型{}缺少处理这些子类型: {}", parent_type, missing_subtypes.join(", ")));
                }
            }
        
            // 收集所有标签信息
            let mut additional_labels = Vec::new();
            for (type_path, loc) in pattern_locations.iter() {
                additional_labels.push((*loc, format!("此模式匹配类型 `{}`", type_path.clone())));
            }
        
            // 收集所有需要添加的模式
            let mut all_patterns = Vec::new();
            let mut help_messages = Vec::new();
        
            // 处理完全未匹配的类型
            if !missing_types.is_empty() {
                let suggestions = missing_types.iter()
                    .map(|t| format!("{}(_)", t))
                    .collect::<Vec<_>>()
                    .join(" 或 ");
            
                help_messages.push(format!("请考虑添加以下模式: {}", suggestions));
            
                // 添加缺失类型的匹配模式
                for missing_type in &missing_types {
                    all_patterns.push(format!("{}(_) => {{}}", missing_type));
                }
            }
        
            // 处理部分匹配的类型
            for (parent_type, missing_subtypes) in &partially_matched_types {
                let suggestions = missing_subtypes.iter()
                    .map(|t| format!("{}({}(_))", parent_type, t))
                    .collect::<Vec<_>>()
                    .join(" 或 ");
            
                help_messages.push(format!("请考虑添加以下模式: {}", suggestions));
            
                // 添加缺失子类型的匹配模式
                for missing_subtype in missing_subtypes {
                    all_patterns.push(format!("{}({}(_)) => {{}}", parent_type, missing_subtype));
                }
            }
        
            // 使用通用方法报告错误
            self.report_non_exhaustive_pattern(
                ctx,
                format!("此匹配不穷尽。{}", error_msg),
                all_patterns,
                match_end_pos,
                indent,
                if help_messages.is_empty() { None } else { Some(help_messages.join("\n")) },
                if additional_labels.is_empty() { None } else { Some(additional_labels) },
            );
        }
    }

    /// 递归收集所有匹配的联合类型，同时记录模式位置
    ///
    /// # 参数
    /// * `ctx` - 编译上下文
    /// * `cond` - 要检查的匹配条件
    /// * `matched_exact_types` - 精确匹配的类型路径列表
    /// * `matched_full_types` - 完整匹配的联合类型列表
    /// * `pattern_locations` - 各模式位置的哈希映射
    pub(crate) fn collect_matched_union_types_with_loc<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        cond: &MatchArmCondition,
        matched_exact_types: &mut Vec<String>,
        matched_full_types: &mut Vec<String>,
        pattern_locations: &mut FxHashMap<String, Range>
    ) {
        match cond {
            MatchArmCondition::Discard(_) => {
                // 通配符匹配所有类型，但这在has_wildcard_pattern中已经检查
            },
            MatchArmCondition::Var(_) => {
                // 变量绑定匹配所有类型，但不是特定类型
            },
            MatchArmCondition::TypedVar(type_node, sub_cond) => {
                if let Ok(t) = type_node.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                    let type_name = t.borrow().get_name().to_string();
                    let range = type_node.range();
                
                    // 记录类型位置
                    pattern_locations.insert(type_name.clone(), range);
                
                    // 检查是否为联合类型，如果是且有子条件为通配符，则认为匹配了所有子类型
                    let is_wildcard_subcond = match &**sub_cond {
                        MatchArmCondition::Var(_) | MatchArmCondition::Discard(_) => true,
                        _ => false
                    };
                
                    if is_wildcard_subcond {
                        // 如果子条件是通配符，则认为完全匹配了此类型
                        matched_full_types.push(type_name.clone());
                        matched_exact_types.push(type_name.clone());
                    
                        // 对于联合类型，添加所有可能的子类型组合
                        if let PLType::Union(inner_union) = &*t.borrow() {
                            let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                            for inner_type in inner_types {
                                let inner_name = inner_type.borrow().get_name().to_string();
                                matched_exact_types.push(inner_name);
                            }
                        }
                    } else {
                        // 子条件不是通配符，处理嵌套类型
                        match &**sub_cond {
                            MatchArmCondition::TypedVar(inner_type, _) => {
                                // 显式的嵌套类型匹配，例如 A1(i32(x))
                                if let Ok(inner_t) = inner_type.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                                    let inner_name = inner_t.borrow().get_name().to_string();
                                    let inner_range = inner_type.range();
                                    let full_path = format!("{}({})", type_name, inner_name);
                                
                                    // 记录嵌套类型位置
                                    pattern_locations.insert(full_path.clone(), inner_range);
                                
                                    // 记录完整的类型路径
                                    matched_exact_types.push(full_path);
                                
                                    // 特殊处理: 如果这是一个子类型，记录它对父类型的贡献
                                    // 例如: 如果我们有A1(i32)和A1(i64)，那么A1本身也被完全匹配了
                                    if let PLType::Union(inner_union) = &*t.borrow() {
                                        let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                                        let mut all_matched = true;
                                        let mut matched_inner_names = vec![inner_name];
                                    
                                        // 检查此联合类型的所有成员是否都被匹配
                                        for other_arm in &self.arms {
                                            if let (MatchArmCondition::TypedVar(other_type, other_subcond), _) = other_arm {
                                                if let Ok(other_t) = other_type.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                                                    if other_t.borrow().get_name() == t.borrow().get_name() {
                                                        // 同一联合类型的另一个分支
                                                        if let MatchArmCondition::TypedVar(other_inner, _) = &**other_subcond {
                                                            if let Ok(other_inner_t) = other_inner.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                                                                matched_inner_names.push(other_inner_t.borrow().get_name().to_string());
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    
                                        // 检查是否所有子类型都被匹配
                                        for inner_type in inner_types {
                                            let inner_type_name = inner_type.borrow().get_name().to_string();
                                            if !matched_inner_names.contains(&inner_type_name) {
                                                all_matched = false;
                                                break;
                                            }
                                        }
                                    
                                        if all_matched {
                                            // 如果该联合类型的所有成员都被匹配了，记录该类型为完全匹配
                                            matched_full_types.push(type_name.clone());
                                            matched_exact_types.push(type_name);
                                        }
                                    }
                                }
                            },
                            _ => {
                                // 其他类型的匹配，只记录外层类型
                                matched_exact_types.push(type_name);
                            }
                        }
                    }
                
                    // 继续递归检查
                    self.collect_matched_union_types_with_loc(ctx, sub_cond, matched_exact_types, matched_full_types, pattern_locations);
                }
            },
            MatchArmCondition::TypedDeconstruct(type_node, fields) => {
                if let Ok(t) = type_node.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                    let type_name = t.borrow().get_name().to_string();
                    let range = type_node.range();
                
                    // 记录类型位置
                    pattern_locations.insert(type_name.clone(), range);
                    matched_exact_types.push(type_name);
                
                    // 递归检查字段条件
                    for (_, sub_cond) in fields {
                        self.collect_matched_union_types_with_loc(ctx, sub_cond, matched_exact_types, matched_full_types, pattern_locations);
                    }
                }
            },
            MatchArmCondition::Literal(_) => {
                // 字面量不直接匹配联合类型
            },
            MatchArmCondition::Deconstruct(fields,_) => {
                // 对于结构体解构，递归检查每个字段
                for (_, sub_cond) in fields {
                    self.collect_matched_union_types_with_loc(ctx, sub_cond, matched_exact_types, matched_full_types, pattern_locations);
                }
            },
            MatchArmCondition::Tuple(fields, _) => {
                // 对于元组，递归检查每个元素
                for sub_cond in fields {
                    self.collect_matched_union_types_with_loc(ctx, sub_cond, matched_exact_types, matched_full_types, pattern_locations);
                }
            }
        }
    }

    /// 检查元组类型的穷尽性
    pub(crate) fn check_tuple_exhaustiveness<'a, 'b>(
        &self, 
        ctx: &'b mut Ctx<'a>, 
        s: &crate::ast::pltype::STType,
        ty: &Arc<RefCell<PLType>>,
        match_end_pos: Pos,
        indent: &str
    ) {
        // 检查是否有元组模式
        let tuple_patterns: Vec<_> = self.arms.iter()
            .filter_map(|(cond, _)| {
                if let MatchArmCondition::Tuple(fields, _) = cond {
                    Some(fields)
                } else {
                    None
                }
            })
            .collect();
    
        // 如果没有元组模式但有通配符，则可能是穷尽的
        if tuple_patterns.is_empty() {
            if !self.has_wildcard_pattern() && !self.has_variable_binding() {
                // 创建元组通配符模式作为QuickFix
                let mut element_placeholders = Vec::new();
                for _ in 0..s.fields.len() {
                    element_placeholders.push("_".to_string());
                }

                let tuple_pattern = format!("({}) => {{}}", element_placeholders.join(", "));
                
                self.range
                    .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                    .add_label(
                        self.range,
                        ctx.get_file(),
                        format_label!(
                            "此元组匹配不穷尽，需要添加模式或 `_` 捕获所有情况"
                        ),
                    )
                    .add_help("请考虑添加通配符模式或特定的元组模式")
                    .add_edit(match_end_pos.to(match_end_pos), format!("\n{}{}", indent, tuple_pattern))
                    .add_to_ctx(ctx);
            }
            return;
        }
    
        // 收集所有需要检查的元素及其模式
        let mut all_fields_exhaustive = true;
        let mut element_patterns = Vec::new();

        // 检查元组字段是否匹配完整
        for (i, field_info) in s.fields.iter() {
            let field_idx = i.parse::<usize>().unwrap_or(0);
            
            // 获取字段类型以进行穷尽性检查
            let field_type_result = field_info.typenode.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false);
            if field_type_result.is_err() {
                continue;
            }
            let field_type = field_type_result.unwrap();
            
            // 收集该位置所有已存在的模式
            let mut field_exhaustive = true;
            let mut field_patterns = Vec::new();
            
            // 检查每个元组模式中对应索引的字段
            for fields in &tuple_patterns {
                if field_idx >= fields.len() {
                    field_exhaustive = false;
                    continue;
                }
                
                // 如果有通配符或变量绑定，认为是穷尽的
                if self.is_wildcard_pattern(&fields[field_idx]) || self.is_variable_binding(&fields[field_idx]) {
                    field_exhaustive = true;
                    break;
                }
                
                // 检查特定类型的穷尽性
                match &*field_type.borrow() {
                    PLType::Primitive(PriType::BOOL) => {
                        // 对于布尔类型，检查是否匹配了true和false
                        let is_true = self.matches_literal_bool(&fields[field_idx], true);
                        let is_false = self.matches_literal_bool(&fields[field_idx], false);
                        
                        if is_true {
                            field_patterns.push("true".to_string());
                        }
                        if is_false {
                            field_patterns.push("false".to_string());
                        }
                        
                        if !is_true || !is_false {
                            field_exhaustive = false;
                        }
                    },
                    _ => {
                        // 其他类型目前不做特殊处理，标记为非穷尽
                        field_exhaustive = false;
                    }
                }
            }
            
            // 如果该位置不穷尽，添加需要的模式
            if !field_exhaustive && !self.has_wildcard_pattern() && !self.has_variable_binding() {
                all_fields_exhaustive = false;
                
                // 根据字段类型添加合适的模式
                match &*field_type.borrow() {
                    PLType::Primitive(PriType::BOOL) => {
                        let mut missing_patterns = Vec::new();
                        if !field_patterns.contains(&"true".to_string()) {
                            missing_patterns.push("true".to_string());
                        }
                        if !field_patterns.contains(&"false".to_string()) {
                            missing_patterns.push("false".to_string());
                        }
                        
                        element_patterns.push(FieldPattern {
                            field_name: field_idx.to_string(),
                            patterns: missing_patterns,
                        });
                    },
                    _ => {
                        // 对于其他类型，添加通配符
                        element_patterns.push(FieldPattern {
                            field_name: field_idx.to_string(),
                            patterns: vec!["_".to_string()],
                        });
                    }
                }
            }
        }
        
        // 如果元组不穷尽，生成QuickFix
        if !all_fields_exhaustive {
            // 生成QuickFix模板
            let mut template_parts = Vec::new();
            for i in 0..s.fields.len() {
                template_parts.push(format!("{}", FIELD_PLACEHOLDER));
            }
            let template = format!("({})", template_parts.join(", "));
            
            // 生成组合模式
            let mut combinations = self.generate_tuple_pattern_combinations(&element_patterns, s.fields.len(), indent);
            
            // 报告错误并添加QuickFix
            self.range
                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                .add_label(
                    self.range,
                    ctx.get_file(),
                    format_label!(
                        "此元组匹配不穷尽，需要添加更多模式"
                    ),
                )
                .add_help("请考虑添加以下模式来确保所有可能的值都被匹配")
                .add_edit(match_end_pos.to(match_end_pos), combinations)
                .add_to_ctx(ctx);
        }
    }

    /// 生成元组模式的所有可能组合
    ///
    /// # 参数
    /// * `field_patterns` - 字段模式列表
    /// * `tuple_size` - 元组大小
    /// * `indent` - 缩进字符串
    ///
    /// # 返回值
    /// 所有可能的模式组合的字符串
    pub(crate) fn generate_tuple_pattern_combinations(
        &self,
        field_patterns: &[FieldPattern],
        tuple_size: usize,
        indent: &str
    ) -> String {
        if field_patterns.is_empty() {
            return String::new();
        }
        
        // 递归生成所有可能的组合
        let combinations = generate_pattern_combinations(field_patterns, 0, Vec::new());
        
        // 生成最终的QuickFix文本
        let mut result = String::new();
        for combo in combinations {
            // 创建一个默认全部是通配符的元组
            let mut pattern_parts = vec!["_".to_string(); tuple_size];
            
            // 应用实际元素
            for (field_idx, pattern) in combo {
                if let Ok(idx) = field_idx.parse::<usize>() {
                    if idx < pattern_parts.len() {
                        pattern_parts[idx] = pattern;
                    }
                }
            }
            
            // 生成最终模式
            let pattern_str = format!("({}) => {{}}", pattern_parts.join(", "));
            result.push_str(&format!("\n{}{}", indent, pattern_str));
        }
        
        result
    }

    /// 检查结构体类型的穷尽性
    pub(crate) fn check_struct_exhaustiveness<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        s: &crate::ast::pltype::STType,
        ty: &Arc<RefCell<PLType>>,
        match_end_pos: Pos,
        indent: &str
    ) {
        // 如果有通配符模式，则一定是穷尽的
        if self.has_wildcard_pattern() {
            return;
        }

        // 收集所有结构体解构模式
        let struct_patterns: Vec<_> = self.arms.iter()
            .filter_map(|(cond, _)| {
                if let MatchArmCondition::Deconstruct(fields,_) = cond {
                    Some(fields)
                } else {
                    None
                }
            })
            .collect();

        // 如果没有结构体解构模式，则不穷尽
        if struct_patterns.is_empty() {
            let mut diag = self.range
                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
            let diag = diag
                .add_label(
                    self.range,
                    ctx.get_file(),
                    format_label!(
                        "此结构体匹配不穷尽，需要添加解构模式或 `_` 捕获所有情况"
                    ),
                );

            // 添加结构体类型名称和建议
            let type_name = s.name.clone();
            diag.add_help(&format!(
                "结构体类型 `{}` 需要匹配模式。请考虑添加 `{{ field1: pattern1, ... }}` 形式的模式或通配符 `_`", 
                type_name
            ));
        
            // 添加quick fix - 在匹配表达式末尾添加通配符模式
            let quick_fix_text = "\n    _ => {}";
            diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text.to_string());
        
            diag.add_to_ctx(ctx);
            return;
        }

        // 收集所有需要检查的字段及其模式
        let mut field_patterns = Vec::new();
        let mut missing_fields = Vec::new();
        let mut all_fields_exhaustive = true;

        // 提取第一个匹配分支的格式作为模板
        let mut template_arm = None;
        if let Some(pattern) = struct_patterns.first() {
            // 复制完整的结构体解构模式，稍后会替换特定字段
            let mut fields_str = Vec::new();
            for (var, _) in *pattern {
                fields_str.push(format!("{}:{}", var.name, FIELD_PLACEHOLDER));
            }
            template_arm = Some(format!("{{{}}}", fields_str.join(",")));
        }

        // 遍历结构体的所有字段
        for (field_name, field_info) in &s.fields {
            let field_name_str = field_name.to_string();
            let field_type_result = field_info.typenode.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false);
            if field_type_result.is_err() {
                continue; // 忽略无法获取类型的字段
            }
            let field_type = field_type_result.unwrap();
        
            // 检查是否所有模式都包含此字段
            let all_patterns_have_field = struct_patterns.iter().all(|pattern| {
                pattern.iter().any(|(field_var, _)| field_var.name == *field_name)
            });
        
            if !all_patterns_have_field {
                missing_fields.push(field_name_str);
                all_fields_exhaustive = false;
                continue;
            }
        
            // 对于每个字段，检查其类型的穷尽性并收集模式
            let patterns_with_field: Vec<_> = struct_patterns.iter()
                .filter_map(|pattern| {
                    pattern.iter()
                        .find(|(field_var, _)| field_var.name == *field_name)
                        .map(|(_, cond)| cond)
                })
                .collect();
            
            match &*field_type.clone().borrow() {
                PLType::Primitive(PriType::BOOL) => {
                    // 布尔字段需要检查true和false是否都被覆盖
                    let has_true = patterns_with_field.iter().any(|cond| {
                        self.matches_literal_bool(cond, true)
                    });
                
                    let has_false = patterns_with_field.iter().any(|cond| {
                        self.matches_literal_bool(cond, false)
                    });
                
                    if !has_true || !has_false {
                        // 收集缺失的布尔值模式
                        let mut missing_patterns = Vec::new();
                        if !has_true {
                            missing_patterns.push(format!("{}:true", field_name_str));
                        }
                        if !has_false {
                            missing_patterns.push(format!("{}:false", field_name_str));
                        }
                        field_patterns.push(FieldPattern {
                            field_name: field_name_str.clone(),
                            patterns: missing_patterns,
                        });
                        all_fields_exhaustive = false;
                    }
                },
                PLType::Union(u) => {
                    // 联合类型字段需要检查所有可能的变体
                    // 检查是否至少有一个模式是通配符或变量绑定
                    let has_wildcard = patterns_with_field.iter().any(|cond| {
                        self.is_wildcard_pattern(cond)
                    });
                    
                    if !has_wildcard {
                        // 收集所有已匹配的类型
                        let mut matched_types = Vec::new();
                        let mut matched_full_types = Vec::new();
                        
                        // 检查所有结构体模式中的这个字段的所有条件
                        for cond in &patterns_with_field {
                            self.collect_matched_union_types_with_loc(ctx, cond, &mut matched_types, &mut matched_full_types, &mut FxHashMap::default());
                        }
                        
                        // 获取联合类型的所有可能变体
                        let sum_types = u.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                        let mut unmatched_types = Vec::new();
                        
                        // 进行穷尽性检查
                        for sum_type in &sum_types {
                            let type_name = sum_type.borrow().get_name().to_string();
                            
                            // 如果这个类型已被完全匹配(例如A1类型被A1(i32)和A1(i64)完全覆盖)，则跳过
                            if matched_full_types.contains(&type_name) {
                                continue;
                            }
                            
                            // 检查是否直接匹配了这个类型
                            if matched_types.iter().any(|t| *t == type_name) {
                                continue;
                            }
                            
                            // 对于嵌套的联合类型(如A1 = i32|i64)，检查其所有成员是否都被匹配
                            if let PLType::Union(inner_union) = &*sum_type.borrow() {
                                let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                                let mut all_inner_matched = true;
                                let mut missing_inner_types = Vec::new();
                                
                                for inner_type in &inner_types {
                                    let inner_name = inner_type.borrow().get_name().to_string();
                                    let full_path = format!("{}({})", type_name, inner_name);
                                    
                                    if !matched_types.iter().any(|t| *t == full_path) {
                                        all_inner_matched = false;
                                        missing_inner_types.push(inner_name);
                                    }
                                }
                                
                                // 如果内部所有类型都被匹配，则认为外部类型也被匹配
                                if all_inner_matched {
                                    continue;
                                }
                                
                                // 如果只有部分内部类型未匹配，添加它们到未匹配类型列表中
                                if !missing_inner_types.is_empty() {
                                    for inner_type in missing_inner_types {
                                        unmatched_types.push(format!("{}({})", type_name, inner_type));
                                    }
                                    continue;
                                }
                            }
                            
                            // 如果所有检查都未通过，则记录这个未匹配的类型
                            unmatched_types.push(type_name);
                        }
                        
                        if !unmatched_types.is_empty() {
                            // 生成未匹配类型的模式
                            let mut missing_patterns = Vec::new();
                            
                            for unmatched_type in unmatched_types {
                                // 生成适当格式的模式
                                let pattern = if unmatched_type.contains("(") {
                                    // 嵌套类型，例如 "A1(i64)"
                                    if unmatched_type.ends_with(")") && !unmatched_type.contains("(x)") && !unmatched_type.contains("(_)") {
                                        if let Some(last_open_index) = unmatched_type.rfind('(') {
                                            let (prefix, inner_type) = unmatched_type.split_at(last_open_index + 1);
                                            let inner_type = inner_type.trim_end_matches(')');
                                            format!("{}:{}(_))", field_name_str, prefix.to_string() + inner_type)
                                        } else {
                                            format!("{}:{}(_)", field_name_str, unmatched_type)
                                        }
                                    } else {
                                        format!("{}:{}", field_name_str, unmatched_type)
                                    }
                                } else {
                                    // 非嵌套类型，添加变量绑定
                                    format!("{}:{}(x)", field_name_str, unmatched_type)
                                };
                                
                                missing_patterns.push(pattern);
                            }
                            
                            // 添加到字段模式列表
                            field_patterns.push(FieldPattern {
                                field_name: field_name_str.clone(),
                                patterns: missing_patterns,
                            });
                            all_fields_exhaustive = false;
                        }
                    }
                },
                PLType::Struct(inner_s) if !inner_s.is_tuple => {
                    // 嵌套结构体的处理
                    let has_inner_wildcard = patterns_with_field.iter().any(|cond| {
                        self.is_wildcard_pattern(cond)
                    });
                    
                    if !has_inner_wildcard {
                        // 递归检查内部结构体的解构模式
                        let inner_deconstruct_patterns: Vec<_> = patterns_with_field.iter()
                            .filter_map(|cond| {
                                if let MatchArmCondition::Deconstruct(fields,_) = cond {
                                    Some(fields)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        
                        if inner_deconstruct_patterns.is_empty() {
                            // 如果没有解构模式，创建一个简单的解构模式
                            let mut field_deconstruct = String::new();
                            
                            // 构建一个包含内部结构体所有字段的解构模式
                            let mut field_placeholders = Vec::new();
                            for (inner_field_name, _) in &inner_s.fields {
                                field_placeholders.push(format!("{}: _", inner_field_name));
                            }
                            
                            if !field_placeholders.is_empty() {
                                field_deconstruct = format!("{}:{{ {} }}", field_name_str, field_placeholders.join(", "));
                            } else {
                                field_deconstruct = format!("{}:{{ }}", field_name_str);
                            }
                            
                            field_patterns.push(FieldPattern {
                                field_name: field_name_str.clone(),
                                patterns: vec![field_deconstruct],
                            });
                            all_fields_exhaustive = false;
                        } else {
                            // 收集内部结构体中未匹配的字段
                            let mut missing_inner_fields = Vec::new();
                            
                            for (inner_field_name, _) in &inner_s.fields {
                                // 检查每个解构模式是否都包含这个内部字段
                                let all_patterns_have_inner_field = inner_deconstruct_patterns.iter().all(|pattern| {
                                    pattern.iter().any(|(field_var, _)| field_var.name == *inner_field_name)
                                });
                                
                                if !all_patterns_have_inner_field {
                                    missing_inner_fields.push(inner_field_name.to_string());
                                }
                            }
                                
                            if !missing_inner_fields.is_empty() {
                                // 构建包含缺失字段的解构模式
                                let mut field_items = Vec::new();
                                for inner_field in &missing_inner_fields {
                                    field_items.push(format!("{}: _", inner_field));
                                }
                                
                                // 为了保持模式的完整性，我们也添加其他字段
                                for (inner_field_name, _) in &inner_s.fields {
                                    let inner_field_str = inner_field_name.to_string();
                                    if !missing_inner_fields.contains(&inner_field_str) {
                                        field_items.push(format!("{}: _", inner_field_str));
                                    }
                                }
                                
                                let deconstruct_pattern = format!("{}:{{ {} }}", field_name_str, field_items.join(", "));
                                field_patterns.push(FieldPattern {
                                    field_name: field_name_str.clone(),
                                    patterns: vec![deconstruct_pattern],
                                });
                                all_fields_exhaustive = false;
                            }
                        }
                    }
                },
                _ => {
                    // 对于其他类型字段，检查是否有通配符或变量绑定
                    let has_wildcard = patterns_with_field.iter().any(|cond| {
                        self.is_wildcard_pattern(cond)
                    });
                    
                    let has_var_binding = patterns_with_field.iter().any(|cond| {
                        self.is_variable_binding(cond)
                    });
                    
                    if !has_wildcard && !has_var_binding {
                        // 如果没有通配符或变量绑定，添加一个简单的通配符模式
                        field_patterns.push(FieldPattern {
                            field_name: field_name_str.clone(),
                            patterns: vec![format!("{}:_", field_name_str)],
                        });
                        all_fields_exhaustive = false;
                    }
                }
            }
        }
        
        // 如果有缺失字段，生成针对它们的模式
        for field_name in &missing_fields {
            field_patterns.push(FieldPattern {
                field_name: field_name.clone(),
                patterns: vec![format!("{}:_", field_name)],
            });
        }
        
        // 如果有未穷尽的字段，生成组合式QuickFix
        if !all_fields_exhaustive {
            let mut diag = self.range.new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
            
            // 添加主标签
            diag.add_label(
                self.range,
                ctx.get_file(),
                format_label!(
                    "此结构体匹配不穷尽，部分字段的模式不完整"
                ),
            );
            
            // 添加帮助信息
            if !missing_fields.is_empty() {
                diag.add_help(&format!(
                    "缺少这些字段: {}",
                    missing_fields.join(", ")
                ));
            }
            
            // 生成组合式QuickFix
            let quick_fix_text = if let Some(template) = template_arm {
                generate_combined_patterns(&field_patterns, &template, indent)
            } else {
                // 如果没有模板，使用简单的通配符匹配
                "\n    _ => {}".to_string()
            };
            
            // 添加QuickFix
            diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
            diag.add_to_ctx(ctx);
        }
    }

    /// 递归检查结构体字段的穷尽性，可以处理任意深度的嵌套结构体
    pub(crate) fn check_struct_field_exhaustiveness_recursive<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        field_path: &str,
        struct_type: &crate::ast::pltype::STType,
        deconstruct_patterns: &Vec<&Vec<(VarNode, MatchArmCondition)>>
    ) {
        // 检查结构体的每个字段
        for (field_name, field_info) in &struct_type.fields {
            // 检查每个解构模式是否都包含这个字段
            let missing_field = deconstruct_patterns.iter().any(|fields| {
                !fields.iter().any(|(field_var, _)| field_var.name == *field_name)
            });
        
            // 生成当前字段的完整路径，使用更直观的格式
            let field_name_str = field_name.to_string();
            let current_field_path = if field_path.is_empty() {
                field_name_str.clone()
            } else {
                format!("{} → {}", field_path, field_name_str)
            };
        
            if missing_field {
                let mut diag = self.range
                    .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                let mut diag = diag
                    .add_label(
                        self.range,
                        ctx.get_file(),
                        format_label!(
                            "此结构体匹配不穷尽，字段 `{}` 在某些模式中缺失",
                            current_field_path
                        ),
                    );
                diag.add_to_ctx(ctx);
                return;
            }
        
            // 获取字段的类型
            let field_type_result = field_info.typenode.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false);
            if field_type_result.is_err() {
                continue; // 忽略无法获取类型的字段
            }
            let field_type = field_type_result.unwrap();
        
            // 为字段收集所有匹配模式
            let field_patterns: Vec<_> = deconstruct_patterns.iter()
                .filter_map(|fields| {
                    fields.iter()
                        .find(|(field_var, _)| field_var.name == *field_name)
                        .map(|(_, cond)| cond)
                })
                .collect();
        
            // 检查是否有通配符模式
            let has_wildcard = field_patterns.iter().any(|cond| {
                self.is_wildcard_pattern(cond)
            });
        
            // 首先检查是否所有模式都是TypedVar，如果是，这可能是一个联合类型
            let all_typed_var = field_patterns.iter().all(|cond| {
                match cond {
                    MatchArmCondition::TypedVar(_, _) => true,
                    _ => false
                }
            });
        
            // 如果全是TypedVar模式，则按照联合类型处理
            if all_typed_var && !field_patterns.is_empty() {
                // 收集所有可能匹配的类型
                let mut matched_types = Vec::new();
                let mut matched_full_types = Vec::new();
            
                for cond in &field_patterns {
                    if let MatchArmCondition::TypedVar(type_node, _) = cond {
                        if let Ok(t) = type_node.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                            let type_name = t.borrow().get_name().to_string();
                            matched_types.push(type_name.clone());
                            matched_full_types.push(type_name);
                        }
                    }
                }
            
                // 尝试从字段类型判断是否可能是联合类型的包装
                if let PLType::Struct(s) = &*field_type.borrow() {
                    // 可能性1: 字段是联合类型别名
                    if let Some(typedef_name) = field_type.borrow().get_name().strip_prefix("type ") {
                        // 检查是否有匹配该类型的所有变体
                        if s.name.contains("|") {
                            let variants: Vec<&str> = s.name.split('|').collect();
                            for variant in variants {
                                let variant = variant.trim();
                                if !matched_types.iter().any(|t| t == variant) {
                                    self.range
                                        .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                        .add_label(
                                            self.range,
                                            ctx.get_file(),
                                            format_label!(
                                                "此结构体匹配不穷尽，字段 `{}` 是联合类型，未匹配变体 `{}`",
                                                current_field_path,
                                                variant
                                            ),
                                        )
                                        .add_to_ctx(ctx);
                                    return;
                                }
                            }
                        }
                    }
                
                    // 可能性2: 检查字段名和类型是否表明这是联合类型
                    let type_info = field_info.typenode.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false);
                    if let Ok(t) = type_info {
                        if let PLType::Union(u) = &*t.borrow() {
                            let sum_types = u.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                        
                            // 检查每个联合类型成员是否都被匹配
                            let mut unmatched_types = Vec::new();
                        
                            for sum_type in &sum_types {
                                let type_name = sum_type.borrow().get_name().to_string();
                            
                                if !matched_types.iter().any(|t| *t == type_name) {
                                    unmatched_types.push(type_name);
                                }
                            }
                        
                            if !unmatched_types.is_empty() {
                                self.range
                                    .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                    .add_label(
                                        self.range,
                                        ctx.get_file(),
                                        format_label!(
                                            "此结构体匹配不穷尽，字段 `{}` 是联合类型，未匹配这些类型: {}",
                                            current_field_path,
                                            unmatched_types.join(", ")
                                        ),
                                    )
                                    .add_to_ctx(ctx);
                                return;
                            }
                        }
                    }
                }
            
                // 如果以上检查都没有问题，则认为匹配是穷尽的
                continue;
            }
        
            // 如果没有通配符，则根据字段类型进行相应的检查
            if !has_wildcard {
                match &*field_type.borrow() {
                    PLType::Primitive(PriType::BOOL) => {
                        // 布尔字段需要检查true和false是否都被覆盖
                        let has_true = field_patterns.iter().any(|cond| {
                            self.matches_literal_bool(cond, true)
                        });
                    
                        let has_false = field_patterns.iter().any(|cond| {
                            self.matches_literal_bool(cond, false)
                        });
                    
                        if !has_true || !has_false {
                            self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是布尔类型，需要匹配 'true' 和 'false'",
                                        current_field_path
                                    ),
                                )
                                .add_to_ctx(ctx);
                            return;
                        }
                    },
                    PLType::Union(u) => {
                        // 联合类型需要检查所有可能的变体
                        let sum_types = u.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                    
                        // 收集所有已匹配的类型
                        let mut matched_types = Vec::new();
                        let mut matched_full_types = Vec::new();
                    
                        for cond in &field_patterns {
                            self.collect_matched_union_types_with_loc(ctx, cond, &mut matched_types, &mut matched_full_types, &mut FxHashMap::default());
                        }
                    
                        // 检查每个联合类型成员是否都被匹配
                        let mut unmatched_types = Vec::new();
                    
                        for sum_type in &sum_types {
                            let type_name = sum_type.borrow().get_name().to_string();
                        
                            // 如果这个类型已被完全匹配，则跳过
                            if matched_full_types.contains(&type_name) {
                                continue;
                            }
                        
                            // 检查是否直接匹配了这个类型
                            if matched_types.iter().any(|t| *t == type_name) {
                                continue;
                            }
                        
                            // 对于嵌套的联合类型，检查其所有成员是否都被匹配
                            if let PLType::Union(inner_union) = &*sum_type.borrow() {
                                let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                                let mut all_inner_matched = true;
                                let mut missing_inner_types = Vec::new();
                            
                                for inner_type in &inner_types {
                                    let inner_name = inner_type.borrow().get_name().to_string();
                                    let full_path = format!("{}({})", type_name, inner_name);
                                
                                    if !matched_types.iter().any(|t| *t == full_path) {
                                        all_inner_matched = false;
                                        missing_inner_types.push(inner_name);
                                    }
                                }
                            
                                // 如果内部所有类型都被匹配，则认为外部类型也被匹配
                                if all_inner_matched {
                                    continue;
                                }
                            
                                // 如果只有部分内部类型未匹配，添加它们到未匹配类型列表中
                                if !missing_inner_types.is_empty() {
                                    for inner_type in missing_inner_types {
                                        unmatched_types.push(format!("{}({})", type_name, inner_type));
                                    }
                                    continue;
                                }
                            }
                        
                            // 如果所有检查都未通过，则记录这个未匹配的类型
                            unmatched_types.push(type_name);
                        }
                    
                        if !unmatched_types.is_empty() {
                            let mut diag = self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                            let mut diag = diag
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 的这些类型没有被匹配: {}",
                                        current_field_path,
                                        unmatched_types.join(", ")
                                    ),
                                );
                        
                            // 生成具体的修复建议
                            let suggestions = unmatched_types.iter()
                                .map(|t| {
                                    if t.contains("(") {
                                        // 对于嵌套联合类型，给出更具体的示例
                                        format!("{}: {}(_)", field_name, t)
                                    } else {
                                        format!("{}: {}(_)", field_name, t)
                                    }
                                })
                                .collect::<Vec<_>>()
                                .join(" 或 ");
                        
                            diag.add_help(&format!("请考虑添加以下模式: {}", suggestions));
                            diag.add_to_ctx(ctx);
                            return;
                        }
                    },
                    PLType::Struct(inner_s) if inner_s.is_tuple => {
                        // 检查元组模式是否包含所有必要的元素
                        let tuple_patterns: Vec<_> = field_patterns.iter()
                            .filter_map(|cond| {
                                if let MatchArmCondition::Tuple(fields, _) = cond {
                                    Some(fields)
                                } else {
                                    None
                                }
                            })
                            .collect();
                    
                        if tuple_patterns.is_empty() {
                            self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是元组类型，需要添加元组模式",
                                        current_field_path
                                    ),
                                )
                                .add_to_ctx(ctx);
                            return;
                        }
                    
                        // 检查元组每个元素是否都被正确匹配
                        for (i, _) in inner_s.fields.iter() {
                            let element_idx = i.parse::<usize>().unwrap_or(0);
                        
                            // 检查每个元组模式是否都包含此元素
                            for fields in &tuple_patterns {
                                if element_idx >= fields.len() {
                                    self.range
                                        .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                        .add_label(
                                            self.range,
                                            ctx.get_file(),
                                            format_label!(
                                                "此结构体匹配不穷尽，字段 `{}` 的元组第{}个元素未被匹配",
                                                current_field_path,
                                                (element_idx + 1).to_string()
                                            ),
                                        )
                                        .add_to_ctx(ctx);
                                    return;
                                }
                            }
                        }
                    },
                    PLType::Struct(inner_s) => {
                        // 对于嵌套结构体，继续递归检查
                        // 首先收集所有解构模式
                        let nested_deconstruct_patterns: Vec<_> = field_patterns.iter()
                            .filter_map(|cond| {
                                if let MatchArmCondition::Deconstruct(fields,_) = cond {
                                    Some(fields)
                                } else {
                                    None
                                }
                            })
                            .collect();
                    
                        if nested_deconstruct_patterns.is_empty() {
                            self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是结构体类型，需要添加解构模式",
                                        current_field_path
                                    ),
                                )
                                .add_to_ctx(ctx);
                            return;
                        }
                    
                        // 递归检查更深层次的嵌套结构体（使用更新后的路径格式）
                        self.check_struct_field_exhaustiveness_recursive(
                            ctx,
                            &current_field_path,
                            inner_s,
                            &nested_deconstruct_patterns
                        );
                    },
                    _ => {
                        // 对于其他类型字段，检查是否有通配符或变量绑定
                        self.range
                            .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                            .add_label(
                                self.range,
                                ctx.get_file(),
                                format_label!(
                                    "此结构体匹配不穷尽，字段 `{}` 需要通配符或变量绑定",
                                    current_field_path
                                ),
                            )
                            .add_to_ctx(ctx);
                        return;
                    }
                }
            }
        }
    }
}

// 辅助方法，用于获取匹配条件的文本表示
pub(crate) fn get_pattern_text<'a>(cond: &MatchArmCondition, ctx: &mut Ctx<'a>) -> String {
    match cond {
        MatchArmCondition::Discard(_) => "_".to_string(),
        MatchArmCondition::Var(v) => v.name.to_string(),
        MatchArmCondition::Literal(lit) => match lit {
            Literal::Number(n) => n.value.to_string(),
            Literal::String(s) => format!("\"{}\"", s.content),
            Literal::Bool(b) => b.value.to_string(),
        },
        MatchArmCondition::TypedVar(t, c) => {
            if let Ok(ty) = t.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                let type_name = ty.borrow().get_name().to_string();
                match &**c {
                    MatchArmCondition::Var(v) => format!("{}({})", type_name, v.name),
                    _ => format!("{}({})", type_name, get_pattern_text(c,ctx)),
                }
            } else {
                "unknown_type".to_string()
            }
        },
        MatchArmCondition::Tuple(fields, _) => {
            let mut fields_text = Vec::new();
            for field in fields {
                fields_text.push(get_pattern_text(field,ctx));
            }
            format!("({})", fields_text.join(","))
        },
        MatchArmCondition::Deconstruct(fields,_) => {
            let mut fields_text = Vec::new();
            for (var, cond) in fields {
                fields_text.push(format!("{}:{}", var.name, get_pattern_text(cond,ctx)));
            }
            format!("{{{}}}", fields_text.join(","))
        },
        MatchArmCondition::TypedDeconstruct(t, fields) => {
            if let Ok(ty) = t.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                let type_name = ty.borrow().get_name().to_string();
                let mut fields_text = Vec::new();
                for (var, cond) in fields {
                    fields_text.push(format!("{}:{}", var.name, get_pattern_text(cond,ctx)));
                }
                format!("{}({{{}}})", type_name, fields_text.join(","))
            } else {
                "unknown_type".to_string()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_generate_pattern_combinations() {
        // 测试简单字段
        let field_patterns = vec![
            FieldPattern {
                field_name: "a".to_string(),
                patterns: vec!["a:true".to_string(), "a:false".to_string()],
            },
            FieldPattern {
                field_name: "b".to_string(),
                patterns: vec!["b:1".to_string(), "b:2".to_string()],
            },
        ];
        
        let template = format!("{{{}:{},{}:{}}}", "a", FIELD_PLACEHOLDER, "b", FIELD_PLACEHOLDER);
        let indent = "    ";
        
        let result = generate_combined_patterns(&field_patterns, &template, indent);
        
        // 应该生成4个组合 (2×2)
        assert!(result.contains("a:true,b:1"));
        assert!(result.contains("a:true,b:2"));
        assert!(result.contains("a:false,b:1"));
        assert!(result.contains("a:false,b:2"));
    }
    
    #[test]
    fn test_placeholder_replacement() {
        // 测试未替换的占位符会被替换为通配符
        let field_patterns = vec![
            FieldPattern {
                field_name: "a".to_string(),
                patterns: vec!["a:true".to_string()],
            },
        ];
        
        let template = format!("{{{}:{},c:{}}}", "a", FIELD_PLACEHOLDER, FIELD_PLACEHOLDER);
        let indent = "    ";
        
        let result = generate_combined_patterns(&field_patterns, &template, indent);
        
        // 检查'c'字段的占位符被替换为通配符
        assert!(result.contains("a:true,c:_"));
    }
    
    #[test]
    fn test_generate_quick_fix_text() {
        let patterns = vec![
            "Pattern1 => {}".to_string(),
            "Pattern2 => {}".to_string(),
        ];
        let indent = "    ";
        
        let result = generate_quick_fix_text(&patterns, indent);
        
        assert_eq!(result, "\n    Pattern1 => {}\n    Pattern2 => {}");
    }
}
