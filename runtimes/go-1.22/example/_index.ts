// TODO: Remove this and implement it in main.go
export default async function(context: any) {
    const payload = context.req.body;

    const response = await fetch(`https://jsonplaceholder.typicode.com/todos/${payload.id ?? 1}`);
    const todo = await response.json();

    return context.res.json({
        message: 'Hello Open Runtimes 👋',
        todo
    });
}