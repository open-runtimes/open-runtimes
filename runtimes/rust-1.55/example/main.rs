use super::{Response, RequestValue};

pub fn main(req: RequestValue) -> Response {
    return Response::send("test".to_string());
}