(** Best effort to send a notification, not critical so just fail silently *)
let post (tree : Test_tree.t) (url : string) : unit =
  let url = Webhook.url_of_string url in
  let head = Git.get_head () in
  let title = Fmt.str "Test results (commit hash=%s) :octopus:" head in
  let body = Fmt.str "%a" Test_tree.pp_total tree in
  let body = Webhook.default_slack_mrkdwn title body in
  Lwt_main.run @@ Webhook.post_and_forget url body
