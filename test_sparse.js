// Simple sparse switch for testing
function sparseSwitchTest(x) {
    // This should generate a sparse switch pattern
    switch (x) {
        case 100:
            return "hundred";
        case 200:
            return "two hundred";
        case 300:
            return "three hundred";
        default:
            return "other";
    }
}

// Test it
console.log(sparseSwitchTest(100));
console.log(sparseSwitchTest(200));
console.log(sparseSwitchTest(999));