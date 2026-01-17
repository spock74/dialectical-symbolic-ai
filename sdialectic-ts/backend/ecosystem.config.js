module.exports = {
  apps: [{
    name: "s-dialectic-backend",
    script: "./src/server.ts",
    interpreter: "node",
    interpreter_args: "--import tsx",
    watch: ["src", "lisp", "prompts", "logic_reasoning", ".env.local"],
    ignore_watch: ["node_modules", "uploads", "logs"],
    instances: 1,
    autorestart: true,
    max_memory_restart: "1G", // Safety restart if memory leaks
    env: {
      NODE_ENV: "development",
      PORT: 3000
    },
    error_file: "./logs/err.log",
    out_file: "./logs/out.log",
    time: true // Add timestamps to logs
  }]
};
