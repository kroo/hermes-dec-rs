// Test function with closures to see environment variable naming
function outer() {
    let x = 10;
    let y = 20;
    
    function inner() {
        let z = 30;
        return x + y + z;
    }
    
    return inner();
}