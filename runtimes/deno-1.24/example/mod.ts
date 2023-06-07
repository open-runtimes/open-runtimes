import axiod from "https://deno.land/x/axiod/mod.ts";

export default async function(context: any) {
    const payload = context.req.body;

    const todo = (await axiod.get(`https://jsonplaceholder.typicode.com/todos/${payload.id ?? 1}`)).data;

    return context.res.json({
        message: 'Hello Open Runtimes 👋',
        todo
    });
}