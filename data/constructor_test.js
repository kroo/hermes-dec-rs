// Test various constructor patterns
function testConstructors() {
    // Basic Error constructor
    const err1 = new Error("Basic error");
    
    // Array constructor with arguments
    const arr1 = new Array(5);
    const arr2 = new Array(1, 2, 3);
    
    // Object constructor
    const obj1 = new Object();
    
    // Date constructor
    const date1 = new Date();
    const date2 = new Date("2024-01-01");
    
    // RegExp constructor
    const regex1 = new RegExp("test", "gi");
    
    // Custom constructor
    function MyClass(name) {
        this.name = name;
    }
    const inst1 = new MyClass("test");
    
    return {err1, arr1, arr2, obj1, date1, date2, regex1, inst1};
}

// Test constructor in try-catch
function testErrorInCatch() {
    try {
        throw new TypeError("Type error test");
    } catch (e) {
        throw new RangeError("Range error: " + e.message);
    }
}

// Test constructor with multiple arguments
function testMultiArgConstructor() {
    const err = new Error("Error", {cause: "test"});
    const regex = new RegExp("pattern", "flags");
    const arr = new Array(1, 2, 3, 4, 5);
    return {err, regex, arr};
}

testConstructors();
