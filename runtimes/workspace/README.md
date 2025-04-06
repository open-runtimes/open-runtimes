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

The workspace runtime also exposes HTTP endpoints for all operations:

### Terminal Endpoints
- `POST /terminal/updateSize` - Update terminal size
- `POST /terminal/createCommand` - Execute a command

### Filesystem Endpoints
- `POST /fs/createFile` - Create a new file
- `POST /fs/getFile` - Get file contents
- `POST /fs/updateFile` - Update file contents
- `POST /fs/updateFilePath` - Move/rename file
- `POST /fs/deleteFile` - Delete a file
- `POST /fs/createFolder` - Create a new folder
- `POST /fs/getFolder` - Get folder contents
- `POST /fs/updateFolderName` - Rename folder
- `POST /fs/updateFolderPath` - Move folder
- `POST /fs/deleteFolder` - Delete folder

### System Endpoints
- `GET /system/getUsage` - Get system usage statistics

### Git Endpoints
- `POST /git/init` - Initialize git repository
- `POST /git/addRemote` - Add git remote
- `POST /git/setUserName` - Set git user name
- `POST /git/setUserEmail` - Set git user email
- `POST /git/getCurrentBranch` - Get current branch
- `POST /git/status` - Get git status
- `POST /git/add` - Stage files
- `POST /git/commit` - Create commit
- `POST /git/pull` - Pull changes
- `POST /git/push` - Push changes

### Code Endpoints
- `POST /code/format` - Format code
- `POST /code/lint` - Lint code

### Health Check
- `GET /health` - Check server health

All POST endpoints expect a JSON body with the same parameters as their WebSocket counterparts.

## Notes

The workspace runtime exposes both WebSocket and HTTP endpoints:
- WebSocket: `ws://localhost:3000/`
- HTTP: `http://localhost:3000/`

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
