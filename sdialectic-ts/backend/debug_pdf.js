const { PDFParse } = require('pdf-parse');
console.log('PDFParse type:', typeof PDFParse);
console.log('Is valid constructor?', PDFParse.prototype && PDFParse.prototype.constructor === PDFParse);
try {
  const instance = new PDFParse();
  console.log('Instance created');
  console.log('Instance keys:', Object.keys(instance));
} catch(e) {
  console.log('Error creating instance:', e.message);
}

