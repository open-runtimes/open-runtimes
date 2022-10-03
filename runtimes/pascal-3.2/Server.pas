program OpenRuntime;

{$mode objfpc}{$H+}
{$codepage utf8}

uses classes, fphttpserver, httpdefs, fpjson, jsonparser, sysutils, streamio;

type
  TRuntimeRequest = class
    private
    public
      Payload: string;
      Headers: TJSONObject;
      Variables: TJSONObject;
      constructor create(ARequest: TFPHTTPConnectionRequest);
    end;

  TRuntimeResponse = class
    procedure Json(Data: TJSONObject) overload;
    procedure Json(Data: TJSONObject; StatusCode: integer) overload;
    procedure Send(ResponseString: string) overload;
    procedure Send(ResponseString: string; StatusCode: integer) overload;
    private
    public
      Data: TJSONObject;
      ResponseString: string;
      StatusCode: integer;
    end;

  TOpenRuntimeRequestHandler = class
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    end;

var
  OpenRuntimeRequestHandler: TOpenRuntimeRequestHandler;

const
  challengeHeader = 'x-internal-challenge';
  challengeEnv = 'INTERNAL_RUNTIME_KEY';

{ TRuntimeRequest }

constructor TRuntimeRequest.Create(ARequest: TFPHTTPConnectionRequest);
var
  requestBody: TJSONObject;
begin
  requestBody := TJSONObject(GetJSON(ARequest.Content));
  self.Payload := requestBody.Get('payload', '');

  self.Headers := requestBody.Get('headers', TJSONObject.Create);
  self.Variables := requestBody.Get('variables', TJSONObject.Create);
end;

{ TRuntimeResponse }

procedure TRuntimeResponse.Json(Data: TJSONObject);
begin
  self.Data := Data;
  self.StatusCode := 200;
end;

procedure TRuntimeResponse.Json(Data: TJSONObject; StatusCode: integer);
begin
  self.Data := Data;
  self.StatusCode := StatusCode;
end;

procedure TRuntimeResponse.Send(ResponseString: string);
begin
  self.ResponseString := ResponseString;
  self.StatusCode := 200;
end;

procedure TRuntimeResponse.Send(ResponseString: string; StatusCode: integer);
begin
  self.ResponseString := ResponseString;
  self.StatusCode := StatusCode;
end;

{ Don't delete the following line; it's used to sed the user's function into the file }
{ Function Placeholder }

{ Request Handler }

procedure TOpenRuntimeRequestHandler.HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  capturedErr, capturedOut: TStringStream;
  runtimeRequest: TRuntimeRequest;
  runtimeResponse: TRuntimeResponse;
  responseObject: TJSONObject;
begin
  if (ARequest.Method <> 'POST') then
  begin
    AResponse.Code := 405;
    AResponse.Contents.Text := 'Method Not Allowed';
    exit;
  end;

  if (ARequest.GetCustomHeader(challengeHeader) <> GetEnvironmentVariable(challengeEnv)) then
  begin
    AResponse.Code := 401;
    AResponse.Contents.Text := 'Unauthorized';
    exit;
  end;

  capturedErr := TStringStream.Create;
  AssignStream(StdErr, capturedErr);
  Rewrite(StdErr);

  capturedOut := TStringStream.Create;
  AssignStream(Output, capturedOut);
  Rewrite(Output); 

  responseObject := TJSONObject.Create;
  runtimeRequest := TRuntimeRequest.Create(ARequest);
  runtimeResponse := TRuntimeResponse.Create;

  try
    Main(runtimeRequest, runtimeResponse);

    if Assigned(runtimeResponse.Data) then
      responseObject.Add('response', runtimeResponse.Data)
    else
      responseObject.Add('response', runtimeResponse.ResponseString);

    responseObject.Add('stdout', capturedOut.DataString);
    AResponse.Code := runtimeResponse.StatusCode;
  except
    responseObject.Add('stdout', capturedOut.DataString);
    responseObject.Add('stderr', capturedErr.DataString);
    AResponse.Code := 500;
  end;

  AResponse.ContentType := 'application/json';
  AResponse.Contents.Text := responseObject.AsJSON;
end;

{ Entrypoint }

var
  httpServer: TFPHttpServer;
begin
  try
    httpServer := TFPHttpServer.Create(nil);
    httpServer.Active := false;
    httpServer.Port := 3000;
    httpServer.OnRequest := @OpenRuntimeRequestHandler.HandleRequest;
    httpServer.Active := true;
  finally
    httpServer.Free;
  end;
end.
