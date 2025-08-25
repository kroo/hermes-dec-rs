// Test sparse switch pattern
function testSparseSwitch(x) {
    switch (x) {
        case 1:
            console.log("one");
            break;
        case 10:
            console.log("ten");
            break;
        case 100:
            console.log("hundred");
            break;
        case 1000:
            console.log("thousand");
            break;
        default:
            console.log("other");
    }
}

// Test with mixed types
function testMixedSwitch(value) {
    switch (value) {
        case 5:
            return "number five";
        case "hello":
            return "greeting";
        case true:
            return "boolean true";
        case null:
            return "null value";
        default:
            return "unknown";
    }
}

// Test with shared tail
function testSharedTail(code) {
    let result;
    switch (code) {
        case 404:
        case 403:
            result = "not found or forbidden";
            break;
        case 200:
        case 201:
            result = "success";
            break;
        default:
            result = "other status";
    }
    return result;
}

// Call the functions to ensure they're included
testSparseSwitch(10);
testMixedSwitch("hello");
testSharedTail(404);