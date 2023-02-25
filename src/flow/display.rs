use super::*;
use petgraph::{
    visit::IntoNodeReferences,
    visit::{EdgeRef, IntoEdgeReferences},
};

pub struct Dot {
    curly: bool,
}

impl Dot {
    pub fn new(curly: bool) -> Self {
        Dot { curly }
    }
    pub fn generate_from_graph(&self, graph: &Graph, name: &String) -> String {
        let mut res = format!("digraph {} {{\n", name);
        if !self.curly {
            res.push_str("graph [splines=polyline];\n");
        }
        for (id, i) in graph.node_references() {
            match i {
                GraphNodeType::Begin => res.push_str(
                    format!(
                        "D{} [shape=box, style=rounded, label=\"begin\", fontname=\"\"];\n",
                        id.index()
                    )
                    .as_str(),
                ),
                GraphNodeType::End => res.push_str(
                    format!(
                        "{{rank = sink; D{} [shape=box, style=rounded, label=\"end\", fontname=\"\"];}}\n",
                        id.index()
                    )
                    .as_str(),
                ),
                GraphNodeType::Node(str) => res.push_str(
                    format!(
                        "D{} [shape=box, label=\"{}\\l\", fontname=\"\"];\n",
                        id.index(),
                        str.replace('\"', "\\\"").replace('\n',"\\l")
                    )
                    .as_str(),
                ),
                GraphNodeType::Choice(str) => res.push_str(
                    format!(
                        "D{} [shape=diamond, label=\"{}?\\l\", fontname=\"\"];\n",
                        id.index(),
                        str.replace('\"', "\\\"").replace('\n',"\\l")
                    )
                    .as_str(),
                ),
                GraphNodeType::Err(src, msg) => res.push_str(
                    format!(
                        "D{} [shape=box, label=<<FONT>{}</FONT><BR/><FONT COLOR=\"red\">{}</FONT>>, fontname=\"\"];\n",
                        id.index(),
                        src.replace('\"', "\\\"").replace('\n',"\\l"),
                        msg.replace('\"', "\\\"").replace('\n',"\\l")
                    )
                    .as_str(),
                ),
                // GraphNodeType::Dummy => {} // all dummy node will be eliminated
                GraphNodeType::Dummy => {}
            }
        }

        for i in graph.edge_references() {
            match i.weight() {
                EdgeType::Normal => res.push_str(
                    format!("D{} -> D{};\n", i.source().index(), i.target().index()).as_str(),
                ),
                EdgeType::Branch(t) => res.push_str(
                    format!(
                        "D{}:{} -> D{}:n [xlabel={}];\n",
                        i.source().index(),
                        if *t { "s" } else { "e" },
                        i.target().index(),
                        if *t { "Y" } else { "N" }
                    )
                    .as_str(),
                ),
            };
        }
        res.push_str("}\n");
        res
    }
}
