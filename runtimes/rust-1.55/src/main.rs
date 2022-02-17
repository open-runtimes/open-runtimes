#![allow(dead_code)]
#![allow(non_camel_case_types)]

#[macro_use] extern crate rocket;
use serde_json::{json, map::Map};

use rocket::{Request, http::Status, outcome::Outcome, request::{self, FromRequest}, serde::{Deserialize, json::{Json, Value}}};

mod custom;

#[derive(Responder)]
pub enum Response {
    value(Value),
    string(String),
}

impl Response {
    fn json(value: Value) -> Self {
        return Response::value(value);
    }

    fn send<S: Into<String>>(value: S) -> Self {
        return Response::string(value.into());
    }
}

#[derive(Deserialize)]
pub struct RequestValue {
    path: Option<String>,
    file: Option<String>,
    env: Map<String, Value>,
    headers: Option<Map<String, Value>>,
    payload: String
}

#[derive(Debug, Deserialize)]
struct ChallengeKey(String);

#[rocket::async_trait]
impl<'r> FromRequest<'r> for ChallengeKey {
    type Error = Value;

    async fn from_request(request: &'r Request<'_>) -> request::Outcome<Self, Self::Error> {
        let key: String = request.headers().get("x-internal-challenge").collect();

        if key.is_empty() {
            return Outcome::Failure((Status::Unauthorized, json!({ "code": 401, "message": "Unauthorized"})));
        }

        let secret = match std::env::var("INTERNAL_RUNTIME_KEY") {
            Ok(val) => val,
            Err(_) => return Outcome::Failure((Status::InternalServerError, json!({ "code": 500, "message": "Internal Server Error"})))
        };

        if key != secret {
            return Outcome::Failure((Status::Unauthorized, json!({ "code": 401, "message": "Unauthorized"})));
        }

        request::Outcome::Success(ChallengeKey(key))
    }
}

#[catch(default)]
fn default_catcher(status: Status, _request: &Request) -> Value {
    json!({ "code": status.code, "message": status.reason() })
}

#[post("/", format = "json", data = "<value>")]
fn index(value: Json<RequestValue>, _challenge: ChallengeKey) -> Response {
    custom::main::main(value.into_inner())
}

#[launch]
fn rocket() -> _ {
    rocket::build().mount("/", routes![index]).register("/", catchers![default_catcher])
}