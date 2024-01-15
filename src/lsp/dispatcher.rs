use lsp_server::{ExtractError, Message, Request, RequestId};

pub struct Dispatcher(Message);

impl Dispatcher {
    pub fn new(msg: Message) -> Self {
        Dispatcher(msg)
    }
    pub fn on<R, F>(&self, mut f: F) -> &Dispatcher
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
        F: FnMut(RequestId, R::Params),
    {
        if let Message::Request(req) = self.0.clone() {
            let params = cast::<R>(req).map_err(|_| ());
            if let Ok(params) = params {
                log::info!("req: {}", R::METHOD);
                f(params.0, params.1);
            }
        }
        self
    }
    pub fn on_noti<R, F>(&self, mut f: F) -> &Dispatcher
    where
        R: lsp_types::notification::Notification,
        R::Params: serde::de::DeserializeOwned,
        F: FnMut(R::Params),
    {
        if let Message::Notification(req) = self.0.clone() {
            let params = cast_noti::<R>(req).map_err(|_| ());
            if let Ok(params) = params {
                log::info!("noti: {}", R::METHOD);
                f(params);
            }
        }
        self
    }
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_noti<R>(
    req: lsp_server::Notification,
) -> Result<R::Params, ExtractError<lsp_server::Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
