// Test variable hoisting without errors
function testVarHoisting() {
    console.log(x); // undefined - var is hoisted
    var x = 5;
    console.log(x); // 5
    
    // Var in block
    if (true) {
        var y = 10;
    }
    console.log(y); // 10 - var is function-scoped
}

function testLetConst() {
    // Proper let/const usage
    let a = 1;
    const b = 2;
    
    if (true) {
        let c = 3;
        const d = 4;
        console.log(a, b, c, d); // All accessible
    }
    
    console.log(a, b); // Only a, b accessible here
}

function testFunctionHoisting() {
    console.log(hoisted()); // "I am hoisted"
    
    function hoisted() {
        return "I am hoisted";
    }
    
    // Function expression - not hoisted
    console.log(notHoisted); // undefined
    var notHoisted = function() {
        return "I am not hoisted";
    };
    console.log(notHoisted()); // "I am not hoisted"
}