import { spawn } from 'child_process';

async function checkCommand(cmd: string, args: string[]): Promise<boolean> {
  return new Promise((resolve) => {
    const p = spawn(cmd, args);
    p.on('error', () => resolve(false));
    p.on('close', (code) => resolve(code === 0));
  });
}

async function checkOllama() {
  try {
    const res = await fetch('http://127.0.0.1:11434/');
    return res.ok || res.status === 200; // Ollama root returns 200 "Ollama is running"
  } catch (e) {
    return false;
  }
}

async function main() {
  console.log("Health Check:");
  
  const sbcl = await checkCommand('sbcl', ['--version']);
  console.log(`- SBCL Installed: ${sbcl ? 'OK' : 'MISSING'}`);

  const poppler = await checkCommand('pdftoppm', ['-v']);
  console.log(`- Poppler (pdftoppm) Installed: ${poppler ? 'OK' : 'MISSING'}`);

  const ollamaRunning = await checkOllama();
  console.log(`- Ollama Service Running: ${ollamaRunning ? 'OK' : 'NOT RUNNING (Is "ollama serve" active?)'}`);
  
  if (sbcl && poppler && ollamaRunning) {
    console.log("\nSystem is READY.");
    process.exit(0);
  } else {
    console.error("\nSystem has MISSING dependencies.");
    process.exit(1);
  }
}

main();
