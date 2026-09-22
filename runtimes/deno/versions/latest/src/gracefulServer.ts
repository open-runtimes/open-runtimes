// Oak passes its abort signal to Deno.serve, which forcibly closes requests.
// Keep Oak's request tracking, but use the server's graceful close instead.
export function gracefulServer(Server: any): any {
  return class extends Server {
    private closing?: Promise<void>;

    constructor(app: any, { signal, ...options }: any) {
      super(app, options);
      signal?.addEventListener("abort", () => this.close(), { once: true });
    }

    close(): Promise<void> {
      // Oak's native iterator can remain pending after the server drains.
      return this.closing ??= super.close().then(() => Deno.exit(0));
    }
  };
}
