function testInlining() {
    const x = 5;
    const y = "hello";
    const z = true;
    
    // Test inlining into function calls
    console.log(x);
    console.log(y);
    console.log(z);
    
    // Test inlining into method calls
    const obj = { method: function(a) { return a; } };
    obj.method(x);
    obj.method(y);
    
    // Test inlining into arithmetic
    const result = x + 10;
    return result;
}

testInlining();
