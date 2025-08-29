// Test file for GlobalSSA analyzer - closure variable resolution
function outerFunction() {
    const outerVar = "hello";
    const outerNum = 42;
    
    function innerFunction() {
        const innerVar = "world";
        console.log(outerVar + " " + innerVar);  // Should resolve outerVar from parent
        return outerNum;  // Should resolve outerNum from parent
    }
    
    function anotherInner() {
        const localVar = outerVar.toUpperCase();  // Should resolve outerVar
        
        function deeplyNested() {
            // Should resolve outerVar from grandparent
            // Should resolve localVar from parent
            return outerVar + " " + localVar;
        }
        
        return deeplyNested();
    }
    
    return {
        inner: innerFunction,
        another: anotherInner
    };
}

// Test async closure
async function asyncOuter() {
    const asyncVar = "async";
    
    async function asyncInner() {
        await new Promise(r => setTimeout(r, 100));
        return asyncVar;  // Should resolve asyncVar from parent
    }
    
    return asyncInner;
}

// Test generator closure
function* generatorOuter() {
    const genVar = "generator";
    
    function* generatorInner() {
        yield genVar;  // Should resolve genVar from parent
    }
    
    return generatorInner;
}

// Export for testing
globalThis.testClosures = {
    outerFunction,
    asyncOuter,
    generatorOuter
};