function while_test(n) {
  let i = n;  // i starts with parameter value
  
  // This while loop might not execute at all if n >= 3
  while (i < 3) {
    i++;
    console.log(i);
  }
  
  return i;
}