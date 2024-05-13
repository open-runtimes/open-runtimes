require 'httparty'
require 'json'

def main(context)
    action = context.req.headers['x-action'] || ''

    case action
    when 'plaintextResponse'
        return context.res.send('Hello World 👋')
    when 'jsonResponse'
        return context.res.json({ 'json': true, 'message': 'Developers are awesome.' })
    when 'customCharsetResponse'
        return context.res.send('ÅÆ', 200, { 'content-type': 'text/plain; charset=iso-8859-1' })
    when 'multipartResponse'
        return context.res.send("--12345
Content-Disposition: form-data; name=\"partOne\"

Why just have one part?
--12345
Content-Disposition: form-data; name=\"partTwo\"

When you can have two!
--12345--", 200, { 'content-type': 'multipart/form-data; boundary=12345' })
    when 'redirectResponse'
        return context.res.redirect('https://github.com/')
    when 'emptyResponse'
        return context.res.empty()
    when 'noResponse'
        context.res.send('This should be ignored, as it is not returned.')
        return nil
    when 'doubleResponse'
        context.res.send('This should be ignored.')
        return context.res.send('This should be returned.')
    when 'headersResponse'
        return context.res.send('OK', 200, {
            'first-header': 'first-value',
            'second-header': context.req.headers['x-open-runtimes-custom-in-header'] || 'missing',
            'cookie': context.req.headers['cookie'] || 'missing',
            'x-open-runtimes-custom-out-header': 'third-value'
        })
    when 'statusResponse'
        return context.res.send('FAIL', 404)
    when 'requestMethod'
        return context.res.send(context.req.method)
    when 'requestUrl'
        return context.res.json({
            'url': context.req.url,
            'port': context.req.port,
            'path': context.req.path,
            'query': context.req.query,
            'queryString': context.req.query_string,
            'scheme': context.req.scheme,
            'host': context.req.host,
        })
    when 'requestHeaders'
        return context.res.json(context.req.headers)
    when 'requestBodyPlaintext'
        return context.res.send(context.req.body)
    when 'requestBodyJson'
        key1 = nil
        key2 = nil

        if context.req.body.instance_of?(String)
            key1 = 'Missing key'
            key2 = 'Missing key'
        else
            key1 = context.req.body['key1'] || 'Missing key'
            key2 = context.req.body['key2'] || 'Missing key'
        end

        return context.res.json({
            'key1': key1,
            'key2': key2,
            'raw': context.req.body_raw
        })
    when 'envVars'
        return context.res.json({
            'var': ENV['CUSTOM_ENV_VAR'] || nil,
            'emptyVar': ENV['NOT_DEFINED_VAR'] || nil
        })
    when 'logs'
        puts 'Native log'
        context.log('Debug log')
        context.error('Error log')

        context.log(42)
        context.log(4.2)
        context.log(true)

        context.log({ 'objectKey': 'objectValue' })
        context.log([ 'arrayValue' ])

        return context.res.send('')
    when 'library'
        todo = JSON.parse(HTTParty.get('https://jsonplaceholder.typicode.com/todos/' + context.req.body_raw).body)
        return context.res.json({ 'todo': todo })
    when 'binary'
        return context.res.send(context.req.body_raw, 200, { 'content-type': 'text/plain; charset=iso-8859-1' })
    when 'timeout'
        context.log('Timeout start.')
        thread = Thread.new do
            sleep(3)
        end
        thread.join
        context.log('Timeout end.');
        return context.res.send('Successful response.');
    else
        raise 'Unknown action'
    end
end