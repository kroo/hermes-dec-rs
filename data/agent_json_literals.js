// Authored JSON-literal CLI fixture. Compile with hermesc -O -emit-binary.
var literalDocuments = [
    '{"escaped/key":{"items":[{"n":1,"label":"keep first"}]},"zero":-0.0}',
    '{"escaped/key":{"items":[{"n":2,"label":"keep second"}]}}',
    '{"n":18446744073709551617}',
    '{"a":1,"\\u0061":2}'
];
print(literalDocuments);
