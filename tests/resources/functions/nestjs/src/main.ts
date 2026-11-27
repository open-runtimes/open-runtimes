import "reflect-metadata";
import express from "express";
import { NestFactory } from "@nestjs/core";
import { AppModule } from "./app.module";
import { ErrorsFilter } from "./errors.filter";

// Answers the shared Serverless test actions (`x-action` header) the way a NestJS
// app would, so the NestJS helper runs the same suite as Express.

async function bootstrap() {
	const app = await NestFactory.create(AppModule, { bodyParser: false });
	app.use(express.raw({ type: () => true, limit: "20mb" }));
	app.useGlobalFilters(new ErrorsFilter());
	await app.listen(process.env.PORT ?? 3000);
	console.log(`NestJS server started on http://0.0.0.0:${process.env.PORT}`);
}

bootstrap();
