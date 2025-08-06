// Test closure variable resolution
function outer() {
    var x = 10;
    var y = 20;
    
    function inner() {
        var z = 30;
        console.log(x, y, z);  // Accessing outer variables
        
        function deepNested() {
            console.log(x, y, z);  // Accessing variables from multiple levels
            x = x + 1;  // Modifying outer variable
        }
        
        return deepNested;
    }
    
    return inner;
}

// Call the functions to test
var makeInner = outer();
var makeDeep = makeInner();
makeDeep();