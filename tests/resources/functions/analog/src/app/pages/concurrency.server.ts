import { PageServerLoad } from '@analogjs/router';

export const load = async ({
    params, // params/queryParams from the request
    req, // H3 Request
    res, // H3 Response handler
    fetch, // internal fetch for direct API calls,
    event, // full request event
}: PageServerLoad) => {
    for (let i = 1; i <= 3; i++) {
        console.log("Concurrent Log " + i);
        await new Promise(resolve => setTimeout(resolve, 500 + Math.random() * 500));
    }

    return {
        msg: "OK Response",
    };
};