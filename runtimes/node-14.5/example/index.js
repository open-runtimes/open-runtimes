const fetch = require("node-fetch");
const crypto = require("crypto");

module.exports = async (context) => {
    const payload = context.req.body;

    const todo = await fetch(`https://jsonplaceholder.typicode.com/todos/${payload.id ?? 1}`).then(r => r.json());

    return context.res.json({
        message: 'Hello Open Runtimes 👋',
        todo
    });
}