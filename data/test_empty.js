// Test with truly empty branch
function testEmpty(x) {
    if (x > 10) {
        // truly empty - just a jump
    } else {
        return "small";
    }
    // No fallthrough return - let it fall to undefined
}