// Test function to understand switch analysis
function switchWithBreak(x) {
    var result;
    switch (x) {
        case 0:
            result = "zero";
            break;
        case 1:
            result = "one";
            break;
        case 2:
            result = "two";
            break;
        case 3:
            result = "three";
            break;
        case 4:
            result = "four";
            break;
        case 5:
            result = "five";
            break;
        default:
            result = "other";
    }
    return result;
}
EOF < /dev/null