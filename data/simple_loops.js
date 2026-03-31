function simple_loops(obj, arr) {
  let i = 0;
  
  // While loop
  while (i < 3) {
    i++;
    console.log(i);
  }

  // Do-while loop
  do {
    i++;
    console.log(i);
  } while (i < 5);

  // For loop
  for (let j = 0; j < 3; j++) {
    i += j;
    console.log(i);
  }

  // For-in loop (without try-catch)
  for (const key in obj) {
    console.log(key);
  }

  return i;
}