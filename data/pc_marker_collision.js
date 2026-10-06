// Authored regression fixture. Compile with hermesc -O0 -emit-binary.
function markerCollision() {
  return "// HBC function 1, PC 999";
}
globalThis.pcMarkerCollision = markerCollision();
