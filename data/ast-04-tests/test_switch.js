// Test switch statement patterns
function testSwitch(value) {
    switch (value) {
        case 1:
            console.log("One");
            break;
        case 2:
            console.log("Two");
            break;
        case 3:
        case 4:
            console.log("Three or Four");
            break;
        default:
            console.log("Other");
    }
    
    // Switch without default
    switch (value) {
        case "a":
            return "Letter A";
        case "b":
            return "Letter B";
    }
    
    // Switch with fall-through
    var result = "";
    switch (value) {
        case 10:
            result += "ten";
        case 11:
            result += "eleven";
            break;
        case 12:
            result += "twelve";
    }
    return result;
}