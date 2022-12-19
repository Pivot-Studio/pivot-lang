use crate::ast::fmt::FmtBuilder;
use crate::ast::node::program::ProgramNode;
use crate::ast::node::{FmtTrait, NodeEnum};
use petgraph::stable_graph::{NodeIndex, StableDiGraph};
use petgraph::visit::{EdgeRef, IntoNodeReferences};
use petgraph::EdgeDirection;
pub mod display;
pub mod test;
// ANCHOR: nodeandedge
#[derive(Debug, Clone, PartialEq)]
pub enum GraphNodeType {
    Dummy,               // 虚节点
    Begin,               // 起点
    End,                 // 终点
    Node(String),        // 普通节点
    Err(String, String), // 错误节点
    Choice(String),      // 选择节点
}

#[derive(Debug, Clone, Copy)]
pub enum EdgeType {
    Normal,
    Branch(bool), // 分支，带有 Y/N 等标签
}

pub type Graph = StableDiGraph<GraphNodeType, EdgeType>;
// ANCHOR_END: nodeandedge

// ANCHOR: GraphContext
struct GraphContext {
    pub graph: Graph,
    pub break_target: NodeIndex,
    pub continue_target: NodeIndex,
    #[allow(dead_code)]
    pub global_begin: NodeIndex, // 全图起点
    pub global_end: NodeIndex,   // 全图终点
    pub local_source: NodeIndex, // 局部起点
    pub local_sink: NodeIndex,   // 局部终点
}
// ANCHOR_END: GraphContext

#[derive(Clone)]
pub struct GraphWrapper {
    pub name: String,
    pub graph: Graph,
}

impl GraphContext {
    fn new() -> GraphContext {
        let mut graph = Graph::new();
        let begin = graph.add_node(GraphNodeType::Begin);
        let end = graph.add_node(GraphNodeType::End);
        GraphContext {
            graph,
            break_target: begin,
            continue_target: end,
            global_begin: begin,
            global_end: end,
            local_source: begin,
            local_sink: end,
        }
    }
}
// ANCHOR: creategraphs
impl ProgramNode {
    pub fn create_graphs(&self) -> Vec<GraphWrapper> {
        let mut graphs = vec![];
        for func in &self.fntypes {
            if let Some(body) = func.body.clone() {
                let graph = from_ast(Box::new(NodeEnum::STS(body)));
                graphs.push(GraphWrapper {
                    name: func
                        .id
                        .name
                        .clone()
                        .replace(|c: char| !c.is_ascii_alphanumeric(), "_"),
                    graph,
                });
            }
        }
        graphs
    }
}
// ANCHOR_END: creategraphs

fn build_graph(ast: Box<NodeEnum>, context: &mut GraphContext) {
    // local_source -> [...current parsing...] -> local_sink
    let mut builder = FmtBuilder::new();
    let local_source = context.local_source;
    let local_sink = context.local_sink;
    let break_target = context.break_target;
    let continue_target = context.continue_target;
    match *ast {
        NodeEnum::STS(v) => {
            let mut sub_source = context.graph.add_node(GraphNodeType::Dummy);
            let mut sub_sink = context.graph.add_node(GraphNodeType::Dummy);
            context
                .graph
                .add_edge(local_source, sub_source, EdgeType::Normal);
            if v.statements.is_empty() {
                context
                    .graph
                    .add_edge(sub_source, sub_sink, EdgeType::Normal);
            } else {
                // ANCHOR: stsloop
                for i in &v.statements {
                    context.local_source = sub_source;
                    context.local_sink = sub_sink;
                    build_graph(i.clone(), context);
                    if i != v.statements.last().unwrap() {
                        sub_source = sub_sink;
                        sub_sink = context.graph.add_node(GraphNodeType::Dummy);
                    }
                }
                // ANCHOR_END: stsloop
            }
            context
                .graph
                .add_edge(sub_sink, local_sink, EdgeType::Normal);
            context.local_source = local_source;
            context.local_sink = local_sink;
        }
        NodeEnum::Continue(s) => {
            // local_source -> current -> continue_target
            s.format(&mut builder);
            let label = builder.generate().clone();
            let current = context.graph.add_node(GraphNodeType::Node(label));
            context
                .graph
                .add_edge(local_source, current, EdgeType::Normal);
            context
                .graph
                .add_edge(current, context.continue_target, EdgeType::Normal);
        }
        NodeEnum::Break(s) => {
            // local_source -> current -> break_target
            s.format(&mut builder);
            let label = builder.generate().clone();
            let current = context.graph.add_node(GraphNodeType::Node(label));
            context
                .graph
                .add_edge(local_source, current, EdgeType::Normal);
            context
                .graph
                .add_edge(current, context.break_target, EdgeType::Normal);
        }
        NodeEnum::Ret(s) => {
            // local_source -> current -> global_end
            s.format(&mut builder);
            let label = builder.generate().clone();
            let current = context.graph.add_node(GraphNodeType::Node(label));
            context
                .graph
                .add_edge(local_source, current, EdgeType::Normal);
            context
                .graph
                .add_edge(current, context.global_end, EdgeType::Normal);
        }
        NodeEnum::If(s) => {
            // local_source -> cond ---Y--> sub_source -> [...body...] -> sub_sink----------->----\
            //                   \                                                                 \
            //                    \---N--> sub_source1 -> Option<[...otherwise...]> -> sub_sink -> local_sink
            s.cond.format(&mut builder);
            let cond_label = builder.generate().clone();
            let cond = context.graph.add_node(GraphNodeType::Choice(cond_label));
            let sub_source = context.graph.add_node(GraphNodeType::Dummy);
            let sub_sink = context.graph.add_node(GraphNodeType::Dummy);
            context.graph.add_edge(local_source, cond, EdgeType::Normal);
            context
                .graph
                .add_edge(cond, sub_source, EdgeType::Branch(true));
            context
                .graph
                .add_edge(sub_sink, local_sink, EdgeType::Normal);
            context.local_source = sub_source;
            context.local_sink = sub_sink;
            // 调用前构建虚节点作为锚点
            build_graph(Box::new(NodeEnum::STS(*s.then)), context);
            // 调用后要恢复原来的锚点
            context.local_source = local_source;
            context.local_sink = local_sink;

            if let Some(t) = s.els {
                let sub_source1 = context.graph.add_node(GraphNodeType::Dummy);
                context
                    .graph
                    .add_edge(cond, sub_source1, EdgeType::Branch(false));
                context.local_source = sub_source1;
                context.local_sink = sub_sink;
                build_graph(t, context);
                context.local_source = local_source;
                context.local_sink = local_sink;
            } else {
                context
                    .graph
                    .add_edge(cond, local_sink, EdgeType::Branch(false));
            }
        }
        NodeEnum::While(s) => {
            //                 /----N--> local_sink
            //                /
            // local_src -> cond ---Y--> sub_source -> [...body...] -> sub_sink
            //                   \                                         /
            //                    \_______________________________________/
            //                                      <<<
            // continue: jump to cond
            // break: jump to local_sink
            s.cond.format(&mut builder);
            let cond_label = builder.generate().clone();
            let cond = context.graph.add_node(GraphNodeType::Choice(cond_label));
            let sub_source = context.graph.add_node(GraphNodeType::Dummy);
            let sub_sink = context.graph.add_node(GraphNodeType::Dummy);
            context.graph.add_edge(local_source, cond, EdgeType::Normal);
            context
                .graph
                .add_edge(cond, sub_source, EdgeType::Branch(true));
            context
                .graph
                .add_edge(cond, local_sink, EdgeType::Branch(false));
            context.graph.add_edge(sub_sink, cond, EdgeType::Normal);
            context.continue_target = cond;
            context.break_target = local_sink;
            context.local_source = sub_source;
            context.local_sink = sub_sink;
            build_graph(Box::new(NodeEnum::STS(*s.body)), context);
            context.continue_target = continue_target;
            context.break_target = break_target;
            context.local_source = local_source;
            context.local_sink = local_sink;
        }
        NodeEnum::For(s) => {
            //                             /----N--> local_sink
            //                            /
            // local_source -> init -> cond ---Y--> sub_source -> [...body...] -> sub_sink -> opt
            //                           \                                                     /
            //                            \                                                   /
            //                             \_________________________________________________/
            //                                              <<<
            // continue: jump to sub_sink
            // break: jump to local_sink
            s.cond.format(&mut builder);
            let cond_label = builder.generate().clone();
            let pre_label = if let Some(pre) = s.pre {
                let mut builder1 = FmtBuilder::new();
                pre.format(&mut builder1);
                builder1.generate().clone()
            } else {
                String::from("")
            };
            let opt_label = if let Some(opt) = s.opt {
                let mut builder2 = FmtBuilder::new();
                opt.format(&mut builder2);
                builder2.generate().clone()
            } else {
                String::from("")
            };

            let sub_source = context.graph.add_node(GraphNodeType::Dummy);
            let sub_sink = context.graph.add_node(GraphNodeType::Dummy);
            let cond = context.graph.add_node(GraphNodeType::Choice(cond_label));
            let init = context.graph.add_node(GraphNodeType::Node(pre_label));
            let upd = context.graph.add_node(GraphNodeType::Node(opt_label));
            context.graph.add_edge(local_source, init, EdgeType::Normal);
            context.graph.add_edge(init, cond, EdgeType::Normal);
            context
                .graph
                .add_edge(cond, sub_source, EdgeType::Branch(true));
            context
                .graph
                .add_edge(cond, local_sink, EdgeType::Branch(false));
            context.graph.add_edge(sub_sink, upd, EdgeType::Normal);
            context.graph.add_edge(upd, cond, EdgeType::Normal);
            context.continue_target = upd;
            context.break_target = local_sink;
            context.local_source = sub_source;
            context.local_sink = sub_sink;
            build_graph(Box::new(NodeEnum::STS(*s.body)), context);
            context.continue_target = continue_target;
            context.break_target = break_target;
            context.local_source = local_source;
            context.local_sink = local_sink;
        }
        NodeEnum::Def(_)
        | NodeEnum::Assign(_)
        | NodeEnum::PointerOpNode(_)
        | NodeEnum::FuncCall(_)
        | NodeEnum::ArrayElementNode(_)
        | NodeEnum::Take(_) => {
            // local_source -> current -> local_sink
            ast.format(&mut builder);
            let label = builder.generate().clone();
            let current = context.graph.add_node(GraphNodeType::Node(label));
            context
                .graph
                .add_edge(local_source, current, EdgeType::Normal);
            context
                .graph
                .add_edge(current, local_sink, EdgeType::Normal);
        }
        NodeEnum::Comment(_) | NodeEnum::Empty(_) => {
            // local_source -> local_sink
            context
                .graph
                .add_edge(local_source, local_sink, EdgeType::Normal);
        }
        NodeEnum::StErrorNode(e) => {
            // local_source -> ERR -> local_sink
            e.st.format(&mut builder);
            let label = builder.generate().clone();
            let err = context.graph.add_node(GraphNodeType::Err(label, e.err.msg));
            context.graph.add_edge(local_source, err, EdgeType::Normal);
            context.graph.add_edge(err, local_sink, EdgeType::Normal);
        }
        _ => {}
    }
}

/// 去除入度为0的节点
fn remove_zero_in_degree_nodes(graph: &mut Graph) -> bool {
    let nodes: Vec<NodeIndex> = graph
        .node_indices()
        .filter(|i| -> bool {
            *graph.node_weight(*i).unwrap() == GraphNodeType::Dummy
                && graph.edges_directed(*i, EdgeDirection::Incoming).count() == 0
        })
        .collect();
    nodes
        .iter()
        .map(|x| graph.remove_node(*x))
        .any(|x| x.is_some())
}

/// 删除满足条件的节点，成功返回true，否则返回false
fn remove_single_node<F>(graph: &mut Graph, predicate: F) -> bool
where
    F: Fn(NodeIndex, &GraphNodeType) -> bool,
{
    // take first dummy node
    if let Some(node_index) = graph
        .node_references()
        .filter(|(x, t)| predicate(*x, t))
        .map(|(x, _)| x)
        .take(1)
        .next()
    {
        let incoming_edges: Vec<(NodeIndex, EdgeType)> = graph
            .edges_directed(node_index, EdgeDirection::Incoming)
            .map(|x| (x.source(), *x.weight()))
            .collect();
        let neighbors: Vec<NodeIndex> = graph
            .neighbors_directed(node_index, EdgeDirection::Outgoing)
            .collect();
        if neighbors.is_empty() {
            return false;
        }
        let next_node = neighbors[0];
        for (src, edge_type) in incoming_edges {
            // add edge: i.src -> next_node
            graph.add_edge(src, next_node, edge_type);
        }
        graph.remove_node(node_index);
        true
    } else {
        false
    }
}

// ANCHOR: fromast
pub fn from_ast(ast: Box<NodeEnum>) -> Graph {
    let mut ctx = GraphContext::new();
    build_graph(ast, &mut ctx);
    // 删除入度为 0 的节点
    while remove_zero_in_degree_nodes(&mut ctx.graph) {}
    // 删除虚节点
    while remove_single_node(&mut ctx.graph, |_, t| *t == GraphNodeType::Dummy) {}
    // 删除空节点
    let remove_empty_nodes: fn(NodeIndex, &GraphNodeType) -> bool = |_, t| match t {
        GraphNodeType::Node(t) => t.is_empty() || t.trim() == ";",
        _ => false,
    };
    while remove_single_node(&mut ctx.graph, remove_empty_nodes) {}
    ctx.graph
}
// ANCHOR_END: fromast
