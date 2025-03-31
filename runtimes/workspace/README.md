# Workspace Runtime

The runtime extends the Node.js runtime to provide an interactive development environment. It uses [Micro](https://github.com/vercel/micro) as the Web Server and integrates [xterm.js](https://xtermjs.org/) for terminal emulation along with Synapse, a custom Operating system gateway for remote serverless environments.

## Quick Start

1. Create a minimal WebSocket client:

```html
<!DOCTYPE html>
<html>
<body>
    <pre id="output"></pre>
    <input id="command" placeholder="Enter command">
    <button onclick="sendCommand()">Send</button>

    <script>
        const ws = new WebSocket('ws://localhost:8080/terminal');
        
        ws.onopen = () => ws.send(JSON.stringify({
            type: 'terminal',
            operation: 'updateSize',
            params: { cols: 80, rows: 24 }
        }));

        ws.onmessage = (event) => {
            const message = JSON.parse(event.data);
            if (message.type === 'terminal_output') {
                document.getElementById('output').textContent += message.data;
            }
        };

        function sendCommand() {
            const cmd = document.getElementById('command').value;
            ws.send(JSON.stringify({ type: 'terminal_input', data: cmd + '\n' }));
            document.getElementById('command').value = '';
        }
    </script>
</body>
</html>
```

2. Start the workspace:

```bash
docker run -p 8080:8080 -e OPEN_RUNTIMES_SECRET=secret-key --rm -it openruntimes/workspace:v4-22 sh helpers/start.sh
```

3. Open the HTML file in your browser to interact with the terminal.

## Available WebSocket Operations

```javascript
// Terminal
{ type: 'terminal', operation: 'updateSize', params: { cols: 80, rows: 24 } }
{ type: 'terminal_input', data: 'command\n' }

// Filesystem
{ type: 'fs', operation: 'getFile', params: { filepath: 'test.txt' } }
{ type: 'fs', operation: 'createFile', params: { filepath: 'test.txt', content: 'Hello' } }

// System
{ type: 'system', operation: 'getUsage' }
```

## Notes

The workspace runtime exposes WebSocket endpoint through Synapse:
- `ws://localhost:8080/terminal` - For terminal sessions

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
