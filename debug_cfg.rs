use hermes_dec_rs::hbc::HbcFile;
use hermes_dec_rs::cfg::Cfg;

fn main() {
    // Load the HBC file
    let hbc_file = HbcFile::from_file("data/flow_control.hbc").expect("Failed to load HBC file");
    
    // Build CFG for function 0
    let mut cfg = Cfg::new(&hbc_file, 0);
    cfg.build();
    
    // Get the graph
    let graph = cfg.graph();
    
    println!("=== CFG Debug for Function 0 ===");
    println!("Total nodes: {}", graph.node_count());
    println!("Total edges: {}", graph.edge_count());
    println!();
    
    // Print all nodes
    for node in graph.node_indices() {
        let block = &graph[node];
        println!("Node {}: Block {} (PC {}-{})", 
                node.index(), 
                node.index(), 
                block.start_pc(), 
                block.end_pc());
        
        if !block.is_exit() {
            println!("  Instructions:");
            for (i, instr) in block.instructions().iter().enumerate() {
                println!("    {}: {:?}", i, instr.instruction);
            }
        } else {
            println!("  EXIT block");
        }
        println!();
    }
    
    // Print all edges
    println!("=== Edges ===");
    for edge in graph.edge_indices() {
        let (from, to) = graph.edge_endpoints(edge).unwrap();
        let edge_kind = &graph[edge];
        println!("{} -> {} ({:?})", from.index(), to.index(), edge_kind);
    }
} 