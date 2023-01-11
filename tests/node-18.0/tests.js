const fetch = require("node-fetch");

const actions = {
    'plaintextResponse': (context) => {
        return context.res.send('Hello World 👋');
    },
    'jsonResponse': (context) => {
        return context.res.json({ json: true, message: 'Developers are awesome.' });
    }
}

module.exports = async (context) => {
    const action = context.req.headers['x-action'];
    return actions[action](context);
}

