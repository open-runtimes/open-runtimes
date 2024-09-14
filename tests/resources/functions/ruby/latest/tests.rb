require 'httparty'
require 'json'
require 'digest'
require 'base64'

require_relative 'awaited_sleep.rb'

def main(context)
    action = context.req.headers['x-action'] || ''

    case action
    when 'plaintextResponse'
        return context.res.text('Hello World 👋')
    when 'jsonResponse'
        return context.res.json({ 'json': true, 'message': 'Developers are awesome.' })
    when 'customCharsetResponse'
        return context.res.text('ÅÆ', 200, { 'content-type': 'text/plain; charset=iso-8859-1' })
    when 'uppercaseCharsetResponse'
        return context.res.text('ÅÆ', 200, { 'content-type': 'TEXT/PLAIN' })
    when 'multipartResponse'
        return context.res.text("--12345
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
        context.res.text('This should be ignored, as it is not returned.')
        return nil
    when 'doubleResponse'
        context.res.text('This should be ignored.')
        return context.res.text('This should be returned.')
    when 'enforcedHeaders'
        return context.res.json({
            'x-custom': context.req.headers['x-custom'],
            'x-custom-uppercase': context.req.headers['x-custom-uppercase'],
            'x-open-runtimes-custom': context.req.headers['x-open-runtimes-custom'],
        })
    when 'headersResponse'
        return context.res.text('OK', 200, {
            'first-header': 'first-value',
            'second-header': context.req.headers['x-open-runtimes-custom-in-header'] || 'missing',
            'cookie': context.req.headers['cookie'] || 'missing',
            'x-open-runtimes-custom-out-header': 'third-value'
        })
    when 'statusResponse'
        return context.res.text('FAIL', 404)
    when 'requestMethod'
        return context.res.text(context.req.method)
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
    when 'requestBodyText'
        return context.res.text(context.req.body_text)
    when 'requestBodyJson'
        return context.res.json(context.req.body_json)
    when 'requestBodyBinary'
        return context.res.binary(context.req.body_binary)
    when 'requestBodyTextAuto'
        return context.res.text(context.req.body)
    when 'requestBodyJsonAuto'
        return context.res.json(context.req.body)
    when 'binaryResponse1'
        return context.res.binary([0,10,255].pack('C*')) # bytes array
    when 'binaryResponse2'
        return context.res.binary([0,20,255].pack('C*')) # Just a filler
    when 'binaryResponse3'
        return context.res.binary([0,30,255].pack('C*')) # Just a filler
    when 'binaryResponse4'
        return context.res.binary([0,40,255].pack('C*')) # Just a filler
    when 'binaryResponse5'
        return context.res.binary([0,50,255].pack('C*')) # Just a filler
    when 'binaryResponseLarge'
        bytes_body = context.req.body_binary
        hex = Digest::MD5.hexdigest bytes_body
        return context.res.text(hex, 200, {
            'x-method': context.req.method
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

        context.log("Log+With+Plus+Symbol")

        context.log(42)
        context.log(4.2)
        context.log(true)

        context.log({ 'objectKey': 'objectValue' })
        context.log([ 'arrayValue' ])

        return context.res.text('')
    when 'library'
        todo = JSON.parse(HTTParty.get('https://jsonplaceholder.typicode.com/todos/' + context.req.body_raw).body)
        return context.res.json({ 'todo': todo })
    when 'timeout'
        context.log('Timeout start.')
        awaited_sleep()
        context.log('Timeout end.');
        return context.res.text('Successful response.');
    when 'deprecatedMethods'
        return context.res.send(context.req.body_raw);
    when 'deprecatedMethodsUntypedBody'
        return context.res.send(50);
    when 'deprecatedMethodsBytesBody'
        # Buffer from base64. It's content as MD5 is 2a8fdeea08e939e9a7c05653544a1374
        image = Base64.decode64('iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAsSwAALEsBpT2WqQAAAMlQTFRFAAAA/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu+zZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/Tdt/TZv/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZv/TZv/TZu/TZu/TZu/TZu/TZu/TZu/Tdv/TZu/TZuuSxTMwAAAEN0Uk5TABN71PrYkxIu5P/jNyDf60XK3vkOWv11JiVsYazyawE8zInhtgd8bXTitQaw8WcBHMbPXP7pciMWIDLd1yIx5hWA/BXEE2wAAACqSURBVHicXY7PCwFxEMXfkxVKkVDbHrb2YMVBOZM/Xzk52AO1tYpNSdRGfhTzne8qvMM085mZ1yMAiky5wQxAWUZtmSmo8QkrhyeD63J5rxZ5ldvCAWzJoSNfPL+Axhb0jmiSCeCLE2MwSOFyraYdre0M3ko9u5c/EG4U9BL5Xpp2ECsQ04BcAEObjyNG6NvseV5/D1RC5mkl+niX4kuymXD+CzDlozT7gDdmIiQgwIp6VQAAAABJRU5ErkJggg==');

        return context.res.send(image, 200, {
            'content-type': 'image/png'
        });
    when 'spreadOperatorLogs'
        engine = 'open-runtimes'
        context.log("engine:", engine)
        context.error("engine:", engine)
        return context.res.text('OK')
    else
        raise 'Unknown action'
    end
end
