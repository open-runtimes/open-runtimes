use rand::Rng;
use super::{Response, RequestValue};

pub fn main(req: RequestValue) -> Response {
    return Response::json(json!({
        "test": rng.gen_range(0.0..1.0)
    }));
}