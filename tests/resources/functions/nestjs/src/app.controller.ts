import { All, Controller, Get, Param, Query, Req, Res } from "@nestjs/common";
import type { Request, Response } from "express";
import { actions } from "./actions";

@Controller()
export class AppController {
  // NestJS routing, beyond the shared actions
  @Get("users/:id")
  user(@Param("id") id: string, @Query("sort") sort: string) {
    return { id, sort };
  }

  @All("{*path}")
  async action(@Req() req: Request, @Res() res: Response) {
    const action = actions[req.headers["x-action"] as string];
    if (!action) {
      throw new Error("Unknown action");
    }

    await action(req, res);
  }
}
