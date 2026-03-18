const fetch = require("node-fetch");
const crypto = require("crypto");
const fs = require("fs");
const { execSync } = require("child_process");

module.exports = async (context) => {
	const action = context.req.headers["x-action"];

	switch (action) {
		case "plaintextResponse":
			return context.res.text("Hello World 👋");
		case "jsonResponse":
			return context.res.json({
				json: true,
				message: "Developers are awesome.",
			});
		case "customCharsetResponse":
			return context.res.text("ÅÆ", 200, {
				"content-type": "text/plain; charset=iso-8859-1",
			});
		case "uppercaseCharsetResponse":
			return context.res.text("ÅÆ", 200, { "content-type": "TEXT/PLAIN" });
		case "multipartResponse":
			return context.res.text(
				`--12345
Content-Disposition: form-data; name="partOne"

Why just have one part?
--12345
Content-Disposition: form-data; name="partTwo"

When you can have two!
--12345--`,
				200,
				{ "content-type": "multipart/form-data; boundary=12345" },
			);
		case "redirectResponse":
			return context.res.redirect("https://github.com/");
		case "emptyResponse":
			return context.res.empty();
		case "noResponse":
			context.res.text("This should be ignored, as it is not returned.");
			break;
		case "doubleResponse":
			context.res.text("This should be ignored.");
			return context.res.text("This should be returned.");
		case "enforcedHeaders":
			return context.res.json({
				"x-custom": context.req.headers["x-custom"],
				"x-custom-uppercase": context.req.headers["x-custom-uppercase"],
				"x-open-runtimes-custom": context.req.headers["x-open-runtimes-custom"],
			});
		case "headersResponse":
			return context.res.text("OK", 200, {
				"first-header": "first-value",
				"second-header":
					context.req.headers["x-open-runtimes-custom-in-header"] ?? "missing",
				cookie: context.req.headers["cookie"] ?? "missing",
				"x-open-runtimes-custom-out-header": "third-value",
			});
		case "statusResponse":
			return context.res.text("FAIL", 404);
		case "requestMethod":
			return context.res.text(context.req.method);
		case "requestUrl":
			return context.res.json({
				url: context.req.url,
				port: context.req.port,
				path: context.req.path,
				query: context.req.query,
				queryString: context.req.queryString,
				scheme: context.req.scheme,
				host: context.req.host,
			});
		case "requestHeaders":
			return context.res.json(context.req.headers);
		case "requestBodyText":
			return context.res.text(context.req.bodyText);
		case "requestBodyJson":
			return context.res.json(context.req.bodyJson);
		case "requestBodyBinary":
			return context.res.binary(context.req.bodyBinary);
		case "requestBodyTextAuto":
			return context.res.text(context.req.body);
		case "requestBodyJsonAuto":
			return context.res.json(context.req.body);
		case "binaryResponse1":
			return context.res.binary(Buffer.from(Uint8Array.from([0, 10, 255]))); // Buffer
		case "binaryResponse2":
			return context.res.binary(Buffer.from(Uint8Array.from([0, 20, 255]))); // Just a filler
		case "binaryResponse3":
			return context.res.binary(Buffer.from(Uint8Array.from([0, 30, 255]))); // Just a filler
		case "binaryResponse4":
			return context.res.binary(Buffer.from(Uint8Array.from([0, 40, 255]))); // Just a filler
		case "binaryResponse5":
			return context.res.binary(Buffer.from(Uint8Array.from([0, 50, 255]))); // Just a filler
		case "binaryResponseLarge":
			const buffer = Buffer.from(context.req.bodyBinary);
			const hash = crypto.createHash("md5").update(buffer).digest("hex");
			return context.res.text(hash, 200, {
				"x-method": context.req.method,
			});
		case "envVars":
			return context.res.json({
				var: process.env.CUSTOM_ENV_VAR,
				emptyVar: process.env.NOT_DEFINED_VAR ?? null,
			});
		case "logs":
			console.log("Native log");
			context.log("Debug log");
			context.error("Error log");

			context.log("Log+With+Plus+Symbol");

			context.log(42);
			context.log(4.2);
			context.log(true);

			const MAX_INT64 = BigInt("9223372036854775807");
			const MIN_INT64 = BigInt("-9223372036854775808");

			// In JS we intentionally test many edge cases for stringifying
			context.log({
				objectKey: "objectValue",
				min: MIN_INT64,
				max: MAX_INT64,
				now: new Date(),
				set: new Set(["setvalue1", "setvalue2", "setvalue3"]),
				buffer: Uint8Array.from([0, 30, 255]),
				url: new URL("https://appwrite.io/my-awesome-path"),
			});

			context.log(["arrayValue"]);

			context.log(String(new Array(9000).fill("A")));
			context.error(String(new Array(9000).fill("B")));

			return context.res.text("");
		case "library":
			const todo = await fetch(
				`https://dummyjson.com/todos/${context.req.bodyRaw}`,
			).then((r) => r.json());
			return context.res.json({ todo });
		case "timeout":
			context.log("Timeout start.");

			await new Promise((resolve) => {
				setTimeout(resolve, 3000);
			});

			context.log("Timeout end.");
			return context.res.text("Successful response.");
		case "deprecatedMethods":
			return context.res.send(context.req.bodyRaw);
		case "deprecatedMethodsUntypedBody":
			return context.res.send(50);
		case "deprecatedMethodsBytesBody":
			// Buffer from base64. It's content as MD5 is 2a8fdeea08e939e9a7c05653544a1374
			const image = Buffer.from(
				"iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAsSwAALEsBpT2WqQAAAMlQTFRFAAAA/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu+zZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/Tdt/TZv/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZv/TZv/TZu/TZu/TZu/TZu/TZu/TZu/Tdv/TZu/TZuuSxTMwAAAEN0Uk5TABN71PrYkxIu5P/jNyDf60XK3vkOWv11JiVsYazyawE8zInhtgd8bXTitQaw8WcBHMbPXP7pciMWIDLd1yIx5hWA/BXEE2wAAACqSURBVHicXY7PCwFxEMXfkxVKkVDbHrb2YMVBOZM/Xzk52AO1tYpNSdRGfhTzne8qvMM085mZ1yMAiky5wQxAWUZtmSmo8QkrhyeD63J5rxZ5ldvCAWzJoSNfPL+Axhb0jmiSCeCLE2MwSOFyraYdre0M3ko9u5c/EG4U9BL5Xpp2ECsQ04BcAEObjyNG6NvseV5/D1RC5mkl+niX4kuymXD+CzDlozT7gDdmIiQgwIp6VQAAAABJRU5ErkJggg==",
				"base64",
			);

			return context.res.send(image, 200, {
				"content-type": "image/png",
			});
		case "setCookie":
			if (typeof Headers === "undefined") {
				return context.res.text("OK", 200, {
					"set-cookie": ["cookie=value; path=/", "cookie2=value2; path=/"],
					"some-header": "some-value",
				});
			}
			const cookieHeaders = new Headers();
			cookieHeaders.append("set-cookie", "cookie=value; path=/");
			cookieHeaders.append("set-cookie", "cookie2=value2; path=/");
			cookieHeaders.append("some-header", "some-value");
			return context.res.text("OK", 200, cookieHeaders);
		case "setCookie2":
			return context.res.text("OK", 200, {
				"set-cookie": ["cookie=value; path=/", "cookie2=value2; path=/"],
				"some-header": "some-value",
			});
		case "responseObjectText":
			return new Response("Hello World 👋", {
				status: 200,
				headers: { "content-type": "text/plain" },
			});
		case "responseObjectJson":
			return new Response(
				JSON.stringify({ json: true, message: "Developers are awesome." }),
				{
					status: 200,
					headers: { "content-type": "application/json" },
				},
			);
		case "responseObjectBinary":
			return new Response(Uint8Array.from([0, 10, 255]), {
				status: 200,
			});
		case "responseObjectEmpty":
			return new Response(null, { status: 204 });
		case "responseObjectRedirect":
			return Response.redirect("https://github.com/", 301);
		case "responseObjectStatus":
			return new Response("FAIL", { status: 404 });
		case "responseObjectHeaders":
			return new Response("OK", {
				status: 200,
				headers: {
					"first-header": "first-value",
					"second-header": "second-value",
				},
			});
		case "nativeRequest":
			const nativeReq = context.req.asNative();
			return context.res.json({
				url: nativeReq.url,
				method: nativeReq.method,
				body: await nativeReq.text(),
				hasContentType: nativeReq.headers.has("content-type"),
			});
		case "spreadOperatorLogs":
			const engine = "open-runtimes";
			context.log("engine:", engine);
			context.error("engine:", engine);
			return context.res.text("OK");
		case "hiddenFile":
			return context.res.text(
				fs
					.readFileSync("/usr/local/server/src/function/.config/.file")
					.toString(),
			);
		case "errorTest":
			context.log("Before error...");
			throw new Error("Error!");
		case "headlessBrowser":
			const puppeteer = require("puppeteer-core");
			const chromium = require("@sparticuz/chromium");
			const path = await chromium.executablePath();
			execSync(`chmod +x ${path}`);
			const browser = await puppeteer.launch({
				args: [...chromium.args, "--disable-gpu"],
				executablePath: path,
				headless: true,
			});
			const page = await browser.newPage();
			await page.goto("https://astro.build/");
			const screenshotBuffer = await page.screenshot({ type: "png" });
			return context.res.binary(screenshotBuffer, 200, {
				"Content-Type": "image/png; charset=utf-8",
			});
		default:
			throw new Error("Unknown action");
	}
};
