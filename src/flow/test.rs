#[cfg(test)]
mod flowtest {
    use crate::{
        ast::node::NodeEnum,
        flow::{display::Dot, from_ast},
        nomparser::{statement::statement_block, Span},
    };

    #[test]
    fn test_easy_flow_chart() {
        let s = "{
            return;
        }
        ";
        let (_, node) = statement_block(Span::from(s)).unwrap();
        let graph = from_ast(Box::new(NodeEnum::Sts(node)));
        let exp = "digraph test {
            D0 [shape=box, style=rounded, label=\"begin\", fontname=\"\"];
            {rank = sink; D1 [shape=box, style=rounded, label=\"end\", fontname=\"\"];}
            D4 [shape=box, label=\"return\\l\", fontname=\"\"];
            D4 -> D1;
            D0 -> D4;
            }"
        .replace(|c: char| c.is_whitespace(), "");
        let out = Dot::new(true)
            .generate_from_graph(&graph, &"test".to_string())
            .replace(|c: char| c.is_whitespace(), "");
        assert_eq!(3, graph.node_count());
        assert_eq!(2, graph.edge_count());
        assert_eq!(exp, out);
    }
    #[test]
    fn test_error_flow_chart() {
        let s = "{
            let a = 1
            return;
        }
        ";
        let (_, node) = statement_block(Span::from(s)).unwrap();
        let graph = from_ast(Box::new(NodeEnum::Sts(node)));
        let exp = "digraph test{
            D0[shape=box,style=rounded,label=\"begin\",fontname=\"\"];
            {rank=sink;D1[shape=box,style=rounded,label=\"end\",fontname=\"\"];}
            D4[shape=box,label=<<FONT>leta=1</FONT><BR/><FONTCOLOR=\"red\">missingsemi</FONT>>,fontname=\"\"];
            D6[shape=box,label=\"return\\l\",fontname=\"\"];
            D4->D6;
            D6->D1;
            D0->D4;
            }"
            .replace(|c: char| c.is_whitespace(), "");
        let out = Dot::new(true)
            .generate_from_graph(&graph, &"test".to_string())
            .replace(|c: char| c.is_whitespace(), "");
        assert_eq!(4, graph.node_count());
        assert_eq!(3, graph.edge_count());
        assert_eq!(exp, out);
    }
    #[test]
    fn test_if_flow_chart() {
        let s = "{
            if a == 1{
                a = 2;
            }
            return;
        }
        ";
        let (_, node) = statement_block(Span::from(s)).unwrap();
        let graph = from_ast(Box::new(NodeEnum::Sts(node)));
        let exp = "digraph test{
            D0[shape=box,style=rounded,label=\"begin\",fontname=\"\"];
            {rank=sink;D1[shape=box,style=rounded,label=\"end\",fontname=\"\"];}
            D4[shape=diamond,label=\"a==1?\\l\",fontname=\"\"];
            D9[shape=box,label=\"a=2\\l\",fontname=\"\"];
            D11[shape=box,label=\"return\\l\",fontname=\"\"];
            D4:e->D11:n[xlabel=N];
            D9->D11;
            D4:s->D9:n[xlabel=Y];
            D11->D1;D0->D4;
            }"
        .replace(|c: char| c.is_whitespace(), "");
        let out = Dot::new(true)
            .generate_from_graph(&graph, &"test".to_string())
            .replace(|c: char| c.is_whitespace(), "");
        assert_eq!(5, graph.node_count());
        assert_eq!(5, graph.edge_count());
        assert_eq!(exp, out);
    }
    #[test]
    fn test_loop_flow_chart() {
        let s = "{
            for let a = 1; a<3;a=a+1{
                a = a+2;
            }
            return;
        }
        ";
        let (_, node) = statement_block(Span::from(s)).unwrap();
        let graph = from_ast(Box::new(NodeEnum::Sts(node)));
        let exp = "digraph test{
            D0[shape=box,style=rounded,label=\"begin\",fontname=\"\"];
            {rank=sink;D1[shape=box,style=rounded,label=\"end\",fontname=\"\"];}
            D6[shape=diamond,label=\"a<3?\\l\",fontname=\"\"];
            D7[shape=box,label=\"leta=1\\l\",fontname=\"\"];
            D8[shape=box,label=\"a=a+1\\l\",fontname=\"\"];
            D11[shape=box,label=\"a=a+2\\l\",fontname=\"\"];
            D13[shape=box,label=\"return\\l\",fontname=\"\"];
            D6:e->D13:n[xlabel=N];
            D7->D6;
            D11->D8;
            D8->D6;
            D6:s->D11:n[xlabel=Y];
            D13->D1;
            D0->D7;
            }"
        .replace(|c: char| c.is_whitespace(), "");
        let out = Dot::new(true)
            .generate_from_graph(&graph, &"test".to_string())
            .replace(|c: char| c.is_whitespace(), "");
        assert_eq!(7, graph.node_count());
        assert_eq!(7, graph.edge_count());
        assert_eq!(exp, out);
    }
}
