use string_builder::Builder;

pub fn print_tabs(b: &mut Builder, tabs: usize) {
    let tab = "    ";
    b.append("\n");
    b.append(tab.repeat(tabs))
}
