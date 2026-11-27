import { ArgumentsHost, Catch, ExceptionFilter } from "@nestjs/common";
import type { Response } from "express";

// Errors go to the execution's error logs; the client gets an empty 500 (or the
// error's status, such as 413 for a body over the limit).
@Catch()
export class ErrorsFilter implements ExceptionFilter {
	catch(error: any, host: ArgumentsHost) {
		const res = host.switchToHttp().getResponse<Response>();
		console.error(error);
		if (res.headersSent) {
			// Let the chunks already written reach the client, then abort.
			setImmediate(() => res.destroy());
			return;
		}

		res.status(error?.status ?? error?.getStatus?.() ?? 500).end();
	}
}
