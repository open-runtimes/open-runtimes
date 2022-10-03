uses
  fpjson,
  fphttpclient,
  opensslsockets;

procedure Main(var RuntimeRequest: TRuntimeRequest; var RuntimeResponse: TRuntimeResponse);
var
  parsedJson: TJSONData;
  payload, todoJson, response: TJSONObject;
  id, todo: string;
begin
  parsedJson := GetJSON(RuntimeRequest.Payload);
  if parsedJson is TJSONObject then
    payload := TJSONObject(parsedJson)
  else
    payload := TJSONObject.Create;

  id := payload.Get('id', '1');

  todo := TFPHttpClient.SimpleGet('https://jsonplaceholder.typicode.com/todos/' + id);
  todoJSON := TJSONObject(GetJSON(todo));

  response := TJSONObject.Create;
  response.Add('isTest', true);
  response.Add('message', 'Hello Open Runtimes 👋');
  response.Add('header', RuntimeRequest.Headers.Get('x-test-header', ''));
  response.Add('variable', RuntimeRequest.Variables.Get('test-variable', ''));
  response.Add('todo', todoJSON);

  WriteLn('log1');
  WriteLn('{hello: world}');
  WriteLn('[hello, world]');

  RuntimeResponse.Json(response);
end;
