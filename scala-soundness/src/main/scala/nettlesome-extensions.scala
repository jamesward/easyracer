import nettlesome.HttpUrl
import soundness.*

extension (url: HttpUrl)
  def withQuery(query: Optional[Text]): HttpUrl = new HttpUrl(
    origin = url.origin,
    pathText = url.pathText,
    query = query,
    fragment = url.fragment
  )