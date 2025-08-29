// Test file for GlobalSSA analyzer - real closure variable resolution
// This version ensures variables are actually captured, not optimized away

function outerFunction(x) {
    // Use let/var to ensure these are mutable and captured
    let outerVar = "hello" + x;
    let outerNum = 42 + x;
    
    function innerFunction(y) {
        const innerVar = "world";
        // Use and modify captured variables
        outerVar = outerVar + y;
        console.log(outerVar + " " + innerVar);
        return outerNum + y;
    }
    
    function anotherInner(z) {
        // Read captured variable
        const localVar = outerVar.toUpperCase();
        // Modify captured variable
        outerNum = outerNum * z;
        
        function deeplyNested(w) {
            // Access variables from multiple levels
            outerVar = outerVar + w;
            return outerVar + " " + localVar + " " + outerNum;
        }
        
        return deeplyNested;
    }
    
    return {
        inner: innerFunction,
        another: anotherInner,
        getOuter: function() { return outerVar; },
        getNum: function() { return outerNum; }
    };
}

// Simple test of environment capture
function makeCounter() {
    let count = 0;
    
    return {
        increment: function() {
            count++;
            return count;
        },
        decrement: function() {
            count--;
            return count;
        },
        get: function() {
            return count;
        }
    };
}

// Export for testing
globalThis.testClosures = {
    outerFunction,
    makeCounter
};