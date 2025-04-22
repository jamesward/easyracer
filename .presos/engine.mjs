import markdownItCodeSnippetEnhanced from '@gerhobbelt/markdown-it-code-snippet-enhanced'
// It'd be nice if this worked, but it doesn't.
// import markdownMermaid from 'markdown-it-mermaid'
// So we use Kroki instead
import markdownItKroki from '@kazumatu981/markdown-it-kroki'

export default ({ marp }) => marp.use(markdownItCodeSnippetEnhanced).use(markdownItKroki) //.use(markdownMermaid)
