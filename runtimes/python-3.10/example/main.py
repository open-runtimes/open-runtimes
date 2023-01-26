import json
import requests

def main(context):
    payload = context.req.body
    id = str(payload.get('id', 1))

    todo = (requests.get('https://jsonplaceholder.typicode.com/todos/' + id)).json()

    return context.res.json({
        'message': 'Hello Open Runtimes 👋',
        'todo': todo
    })