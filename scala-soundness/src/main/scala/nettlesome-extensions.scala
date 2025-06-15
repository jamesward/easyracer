import nettlesome.Url
import soundness.*

extension [scheme <: Label](url: Url[scheme])
  def withQuery(query: Optional[Text]): Url[scheme] = new Url[scheme](
    origin = url.origin,
    pathText = url.pathText,
    query = query,
    fragment = url.fragment
  )