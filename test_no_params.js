// Test without parameters to avoid LoadParam issues
function testSwitch() {
    var x = 100;
    
    switch (x) {
        case 100:
            console.log("hundred");
            break;
        case 200:
            console.log("two hundred");
            break;
        case 300:
            console.log("three hundred");
            break;
        default:
            console.log("other");
    }
}

testSwitch();