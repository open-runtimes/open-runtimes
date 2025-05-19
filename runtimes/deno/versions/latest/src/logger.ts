export const nativeLog = console.log.bind(console);

export class Logger {
    static TYPE_ERROR = "error";
    static TYPE_LOG = "log";

    static buffers: Map<string, { logs: string[]; errors: string[] }> =
        new Map();
    static #savedConsole: any = null;

    id: string;

    constructor(status?: string, id?: string) {
        this.id = Logger.start(status, id);
    }

    write(messages: unknown[], type = Logger.TYPE_LOG) {
        Logger.write(this.id, messages, type);
    }

    end() {
        return Logger.end(this.id);
    }

    overrideNativeLogs(
        namespace: { get: (key: string) => string } | null = null,
    ) {
        Logger.overrideNativeLogs(namespace);
    }

    revertNativeLogs() {
        Logger.revertNativeLogs();
    }

    // lifecycle
    static start(status?: string, id?: string) {
        if ((status ?? "enabled") !== "enabled") return "";
        if (!id) id = Logger.#generateId();
        if (!Logger.buffers.has(id))
            Logger.buffers.set(id, { logs: [], errors: [] });
        return id;
    }

    static write(id: string, messages: unknown[], type = Logger.TYPE_LOG) {
        const entry = Logger.buffers.get(id);
        if (!entry) return;

        const parts = messages.map((m) =>
            m instanceof Error
                ? (m.stack ?? m).toString()
                : typeof m === "object"
                  ? JSON.stringify(m)
                  : String(m),
        );

        (type === Logger.TYPE_ERROR ? entry.errors : entry.logs).push(
            parts.join(" "),
        );
    }

    static async end(id: string) {
        const entry = Logger.buffers.get(id);
        if (!entry) return;

        await Promise.all([
            Logger.#flush(
                id,
                entry.logs.join("\n") + (entry.logs.length ? "\n" : ""),
                "logs",
            ),
            Logger.#flush(
                id,
                entry.errors.join("\n") + (entry.errors.length ? "\n" : ""),
                "errors",
            ),
        ]);

        Logger.buffers.delete(id);
    }

    // console override
    static overrideNativeLogs(
        namespace: { get: (key: string) => string } | null = null,
    ) {
        if (Logger.#savedConsole) return;

        Logger.#savedConsole = { ...console } as any;

        const proxy =
            (type: string) =>
            (...args: unknown[]) => {
                const id = namespace?.get("id") ?? "dev";
                Logger.write(id, args, type);
            };

        console.log =
            console.info =
            console.debug =
            console.warn =
                proxy(Logger.TYPE_LOG);
        console.error = proxy(Logger.TYPE_ERROR);
    }

    static revertNativeLogs() {
        if (!Logger.#savedConsole) return;
        console.log = Logger.#savedConsole.log;
        console.info = Logger.#savedConsole.info;
        console.debug = Logger.#savedConsole.debug;
        console.warn = Logger.#savedConsole.warn;
        console.error = Logger.#savedConsole.error;
        Logger.#savedConsole = null;
    }

    // internals
    static async #flush(id: string, data: string, suffix: string) {
        if (!data.trim()) return;
        const tmpPath = `/mnt/logs/${id}_${suffix}.tmp`;
        const finalPath = `/mnt/logs/${id}_${suffix}.log`;

        const encoder = new TextEncoder();
        const file = await Deno.open(tmpPath, {
            write: true,
            create: true,
            truncate: true,
        });
        try {
            await file.write(encoder.encode(data));
            await file.sync();
        } finally {
            file.close();
        }

        await Deno.rename(tmpPath, finalPath);
    }

    static #generateId(padding = 7) {
        const now = Date.now();
        const sec = Math.floor(now / 1000).toString(16);
        const msec = (now % 1000).toString(16).padStart(5, "0");
        let rnd = "";
        for (let i = 0; i < padding; i++)
            rnd += Math.floor(Math.random() * 16).toString(16);
        return sec + msec + rnd;
    }
}
