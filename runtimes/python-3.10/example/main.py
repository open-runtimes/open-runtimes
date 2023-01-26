import json
import requests

def main(context):
    const payload = context.req.body
    todo_id = str(payload.get('id', 1))

    todo = (requests.get('https://jsonplaceholder.typicode.com/todos/' + todo_id)).json()

    return context.res.json({
        'message': 'Hello Open Runtimes 👋',
        'todo': todo
    })