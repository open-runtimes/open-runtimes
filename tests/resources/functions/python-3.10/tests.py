import requests
import os
import asyncio

async def main(context):
    action = context.req.headers.get('x-action', None)

    if action == 'plaintextResponse':
        return context.res.text('Hello World 👋')
    elif action == 'jsonResponse':
        return context.res.json({ 'json': True, 'message': 'Developers are awesome.' })
    elif action == 'customCharsetResponse':
        return context.res.text('ÅÆ', 200, { 'content-type': 'text/plain; charset=iso-8859-1' })
    elif action == 'uppercaseCharsetResponse':
        return context.res.text('ÅÆ', 200, { 'content-type': 'TEXT/PLAIN' })
    elif action == 'multipartResponse':
        return context.res.text("""--12345
Content-Disposition: form-data; name=\"partOne\"

Why just have one part?
--12345
Content-Disposition: form-data; name=\"partTwo\"

When you can have two!
--12345--""", 200, { 'content-type': 'multipart/form-data; boundary=12345' })
    elif action == 'redirectResponse':
        return context.res.redirect('https://github.com/')
    elif action == 'emptyResponse':
        return context.res.empty()
    elif action == 'noResponse':
        context.res.text('This should be ignored, as it is not returned.')
    elif action == 'doubleResponse':
        context.res.text('This should be ignored.')
        return context.res.text('This should be returned.')
    elif action == 'headersResponse':
        return context.res.text('OK', 200, {
            'first-header': 'first-value',
            'second-header': context.req.headers.get('x-open-runtimes-custom-in-header', 'missing'),
            'cookie': context.req.headers.get('cookie', 'missing'),
            'x-open-runtimes-custom-out-header': 'third-value'
        })
    elif action == 'statusResponse':
        return context.res.text('FAIL', 404)
    elif action == 'requestMethod':
        return context.res.text(context.req.method)
    elif action == 'requestUrl':
        return context.res.json({
            'url': context.req.url,
            'port': context.req.port,
            'path': context.req.path,
            'query': context.req.query,
            'queryString': context.req.query_string,
            'scheme': context.req.scheme,
            'host': context.req.host,
        })
    elif action == 'requestHeaders':
        return context.res.json(context.req.headers)
    elif action == 'requestBodyText':
        return context.res.text(context.req.body_text)
    elif action == 'requestBodyJson':
        return context.res.json(context.req.body_json)
    elif action == 'requestBodyBinary':
        return context.res.binary(context.req.body_binary)
    elif action == 'requestBodyTextAuto':
        return context.res.text(context.req.body)
    elif action == 'requestBodyJsonAuto':
        return context.res.json(context.req.body)
    elif action == 'requestBodyBinaryAuto':
        return context.res.binary(context.req.body)
    elif action == 'binaryResponse1':
        return context.res.binary(bytearray([0,10,255])) # bytearray
    elif action == 'binaryResponse2':
        return context.res.binary(bytes([0,20,255])) # bytes
    elif action == 'binaryResponse3':
        return context.res.binary(bytearray([0,30,255])) # Just a filler
    elif action == 'binaryResponse4':
        return context.res.binary(bytearray([0,40,255])) # Just a filler
    elif action == 'binaryResponse5':
        return context.res.binary(bytearray([0,50,255])) # Just a filler
    elif action == 'envVars':
        return context.res.json({
            'var': os.environ.get('CUSTOM_ENV_VAR', None),
            'emptyVar': os.environ.get('NOT_DEFINED_VAR', None)
        })
    elif action == 'logs':
        print('Native log')
        context.log('Debug log')
        context.error('Error log')
                
        context.log("Log+With+Plus+Symbol")

        context.log(42)
        context.log(4.2)
        context.log(True)

        context.log({ 'objectKey': 'objectValue' })
        context.log([ 'arrayValue' ])

        return context.res.text('')
    elif action == 'library':
        todo = (requests.get('https://jsonplaceholder.typicode.com/todos/' + context.req.body_raw)).json()
        return context.res.json({
            'todo': todo
        })
    elif action == 'timeout':
        context.log('Timeout start.')
        await asyncio.sleep(3)
        context.log('Timeout end.')
        return context.res.text('Successful response.')
    elif action == 'deprecatedMethods':
        return context.res.send(context.req.body_raw)
    elif action == 'deprecatedMethodsUntypedBody':
        return context.res.send(50)
    else:
        raise Exception('Unknown action')