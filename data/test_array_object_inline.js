function testArrayObjectInlining() {
    // Test array literal inlining
    const arr = [1, 2, 3];
    console.log(arr);
    
    // Test object literal inlining
    const obj = { a: 1, b: 2 };
    console.log(obj);
    
    // Test nested structures
    const nested = { arr: [4, 5], obj: { c: 3 } };
    console.log(nested);
    
    // Test in function calls
    const nums = [10, 20, 30];
    Math.max(...nums);
    
    return arr;
}

testArrayObjectInlining();
