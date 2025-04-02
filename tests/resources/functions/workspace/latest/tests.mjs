import { WebSocket } from "ws";

export default async (context) => {
	const action = context.req.headers["x-action"];
	const WS_URL = "ws://localhost:3000/terminal";

	switch (action) {
		case "websocketConnection":
			try {
				const ws = new WebSocket(WS_URL);

				const result = await new Promise((resolve, reject) => {
					ws.on("open", () => {
						ws.close();
						resolve({
							success: true,
							message: "WebSocket connected successfully",
						});
					});

					ws.on("error", (error) => {
						reject({ success: false, error: error.message });
					});

					// Timeout after 5 seconds
					setTimeout(
						() => reject({ success: false, error: "Connection timeout" }),
						5000,
					);
				});

				return context.res.json(result);
			} catch (error) {
				return context.res.json({
					success: false,
					error: error.message,
				});
			}

		case "terminalOperations":
			try {
				const ws = new WebSocket(WS_URL);

				const result = await new Promise((resolve, reject) => {
					ws.on("open", () => {
						// Send terminal message in Synapse format
						ws.send(
							JSON.stringify({
								type: "terminal",
								operation: "updateSize",
								params: { cols: 80, rows: 24 },
								requestId: "1234",
							}),
						);
					});

					ws.on("message", (data) => {
						const response = JSON.parse(data.toString());
						if (response.type === "terminal_response") {
							ws.close();
							resolve({ success: true, response });
						}
					});

					ws.on("error", (error) => {
						reject({ success: false, error: error.message });
					});

					setTimeout(
						() => reject({ success: false, error: "Operation timeout" }),
						5000,
					);
				});

				return context.res.json(result);
			} catch (error) {
				return context.res.json({
					success: false,
					error: error.message,
				});
			}

		case "fileSystemOperations":
			try {
				const ws = new WebSocket(WS_URL);

				const result = await new Promise((resolve, reject) => {
					ws.on("open", () => {
						// Send fs operation in Synapse format
						ws.send(
							JSON.stringify({
								type: "fs",
								operation: "createFile",
								params: {
									filepath: "test.txt",
									content: "Hello, World!",
								},
								requestId: "5678",
							}),
						);
					});

					ws.on("message", (data) => {
						const response = JSON.parse(data.toString());
						if (response.type === "fs_response") {
							ws.close();
							resolve({ success: true, response });
						}
					});

					ws.on("error", (error) => {
						reject({ success: false, error: error.message });
					});

					setTimeout(
						() => reject({ success: false, error: "Operation timeout" }),
						5000,
					);
				});

				return context.res.json(result);
			} catch (error) {
				return context.res.json({
					success: false,
					error: error.message,
				});
			}

		case "terminalInputOutput":
			try {
				const ws = new WebSocket(WS_URL);

				const result = await new Promise((resolve, reject) => {
					ws.on("open", () => {
						// Send terminal input
						ws.send(
							JSON.stringify({
								type: "terminal_input",
								data: "ls -la\n",
							}),
						);
					});

					let output = "";
					ws.on("message", (data) => {
						const response = JSON.parse(data.toString());
						if (response.type === "terminal_output") {
							output += response.data;
							// Resolve after receiving some output
							if (output.length > 0) {
								ws.close();
								resolve({ success: true, output });
							}
						}
					});

					ws.on("error", (error) => {
						reject({ success: false, error: error.message });
					});

					setTimeout(
						() => reject({ success: false, error: "Operation timeout" }),
						5000,
					);
				});

				return context.res.json(result);
			} catch (error) {
				return context.res.json({
					success: false,
					error: error.message,
				});
			}

		case "systemUsage":
			try {
				const ws = new WebSocket(WS_URL);

				const result = await new Promise((resolve, reject) => {
					ws.on("open", () => {
						ws.send(
							JSON.stringify({
								type: "system",
								operation: "getUsage",
								requestId: "9012",
							}),
						);
					});

					ws.on("message", (data) => {
						const response = JSON.parse(data.toString());
						if (response.type === "system_response") {
							ws.close();
							resolve({ success: true, response });
						}
					});

					ws.on("error", (error) => {
						reject({ success: false, error: error.message });
					});

					setTimeout(
						() => reject({ success: false, error: "Operation timeout" }),
						5000,
					);
				});

				return context.res.json(result);
			} catch (error) {
				return context.res.json({
					success: false,
					error: error.message,
				});
			}

		default:
			throw new Error("Unknown action");
	}
};
