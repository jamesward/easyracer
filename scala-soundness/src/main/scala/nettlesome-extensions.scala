import urticose.Url
import soundness.*

extension [scheme <: Label](url: Url[scheme])
  def withQuery(query: Optional[Text]): Url[scheme] = new Url[scheme](
    origin = url.origin,
    location = url.location,
    query = query,
    fragment = url.fragment
  )