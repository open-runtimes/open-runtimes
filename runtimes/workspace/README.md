# Workspace Runtime

The runtime extends the Node.js runtime to provide an interactive development environment. It uses [Micro](https://github.com/vercel/micro) as the HTTP Server and [Synapse](https://github.com/appwrite/synapse/) as the all in one Operating System gateway for remote serverless environments.

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
        const ws = new WebSocket('ws://localhost:3000/');
        
        ws.onopen = () => ws.send(JSON.stringify({
            type: 'terminal',
            operation: 'updateSize',
            params: { cols: 80, rows: 24 }
        }));

        ws.onmessage = (event) => {
            const message = JSON.parse(event.data);
            if (message.type === 'terminalResponse') {
                document.getElementById('output').textContent += message.data;
            }
        };

        function sendCommand() {
            const cmd = document.getElementById('command').value;
            ws.send(JSON.stringify({ 
                type: 'terminal',
                operation: 'createCommand',
                params: { command: cmd }
            }));
            document.getElementById('command').value = '';
        }
    </script>
</body>
</html>
```

2. Start the workspace:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm -it openruntimes/workspace:v4-1 sh helpers/start.sh
```

3. Open the HTML file in your browser to interact with the terminal.

## Available WebSocket Operations

```javascript
// Terminal Operations
{ type: 'terminal', operation: 'updateSize', params: { cols: 80, rows: 24 } }
{ type: 'terminal', operation: 'createCommand', params: { command: 'ls' } }

// Filesystem Operations
{ type: 'fs', operation: 'createFile', params: { filepath: 'test.txt', content: 'Hello' } }
{ type: 'fs', operation: 'getFile', params: { filepath: 'test.txt' } }
{ type: 'fs', operation: 'updateFile', params: { filepath: 'test.txt', content: 'Updated' } }
{ type: 'fs', operation: 'updateFilePath', params: { filepath: 'test.txt', newPath: 'new.txt' } }
{ type: 'fs', operation: 'deleteFile', params: { filepath: 'test.txt' } }
{ type: 'fs', operation: 'createFolder', params: { folderpath: 'newfolder' } }
{ type: 'fs', operation: 'getFolder', params: { folderpath: 'newfolder' } }
{ type: 'fs', operation: 'updateFolderName', params: { folderpath: 'newfolder', name: 'renamed' } }
{ type: 'fs', operation: 'updateFolderPath', params: { folderpath: 'newfolder', newPath: 'moved' } }
{ type: 'fs', operation: 'deleteFolder', params: { folderpath: 'newfolder' } }

// System Operations
{ type: 'system', operation: 'getUsage' }

// Git Operations
{ type: 'git', operation: 'init' }
{ type: 'git', operation: 'addRemote', params: { name: 'origin', url: 'repo-url' } }
{ type: 'git', operation: 'setUserName', params: { name: 'username' } }
{ type: 'git', operation: 'setUserEmail', params: { email: 'email' } }
{ type: 'git', operation: 'getCurrentBranch' }
{ type: 'git', operation: 'status' }
{ type: 'git', operation: 'add', params: { files: ['file1', 'file2'] } }
{ type: 'git', operation: 'commit', params: { message: 'commit message' } }
{ type: 'git', operation: 'pull' }
{ type: 'git', operation: 'push' }

// Code Operations
{ type: 'code', operation: 'format', params: { code: 'code', options: {} } }
{ type: 'code', operation: 'lint', params: { code: 'code', options: {} } }
```

## HTTP Endpoints

The workspace runtime exposes a single HTTP endpoint for all operations, along with a health check endpoint:

- `POST /` - Main endpoint for all operations
- `GET /health` - Check server health

The POST endpoint accepts JSON payloads with the following structure:
```json
{
    "type": "<operation_type>",
    "operation": "<operation_name>",
    "params": {
        // operation-specific parameters
    }
}
```

Where `type` can be one of: `terminal`, `fs`, `system`, `git`, or `code`.

Example requests:

```json
// Execute a terminal command
{
    "type": "terminal",
    "operation": "createCommand",
    "params": {
        "command": "ls"
    }
}

// Create a file
{
    "type": "fs",
    "operation": "createFile",
    "params": {
        "filepath": "test.txt",
        "content": "Hello"
    }
}

// Get system usage
{
    "type": "system",
    "operation": "getUsage"
}
```

All operations that were previously available as separate endpoints are now handled through this single endpoint with the appropriate payload type and operation.

## Notes

The workspace runtime exposes both WebSocket and HTTP endpoints:
- WebSocket: `ws://localhost:3000/`
- HTTP: `http://localhost:3000/`

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
