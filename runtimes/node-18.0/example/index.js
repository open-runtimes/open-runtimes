module.exports = async (context) => {
    context.log("OK1");
    context.log("OK2");
    context.error("OK3");
    context.error("OK4");

    return context.res.send('Works!!!', 202, { 'x-my-something': 'works' });
}