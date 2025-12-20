const fs = require('fs');

// Simple PDF Header and content construction (text only logic simulation for test)
// Actually creating a real binary PDF is hard without a library.
// We'll use a dummy PDF content that pdf-parse might accept if it's lenient, 
// OR we just use a small base64 string of a valid PDF "Hello World".

// Minimal PDF 1.4 "Hello World"
const pdfContent = `%PDF-1.4
1 0 obj
<< /Type /Catalog /Pages 2 0 R >>
endobj
2 0 obj
<< /Type /Pages /Kids [3 0 R] /Count 1 >>
endobj
3 0 obj
<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 4 0 R >>
endobj
4 0 obj
<< /Length 50 >>
stream
BT
/F1 24 Tf
100 700 Td
(Digoxin test for Lisp verification) Tj
ET
endstream
endobj
xref
0 5
0000000000 65535 f
0000000010 00000 n
0000000060 00000 n
0000000157 00000 n
0000000244 00000 n
trailer
<< /Size 5 /Root 1 0 R >>
startxref
344
%%EOF
`;

fs.writeFileSync('test.pdf', pdfContent);
console.log('Created test.pdf');
