import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult
} from 'vscode-languageserver/node';
import {
	TextDocument
} from 'vscode-languageserver-textdocument';

let currDocument:TextDocument;
let diagnostics: Diagnostic[] = [];

export function validateApi(parsedYAML: any , document: TextDocument) : Diagnostic[]{
	diagnostics = [];
	currDocument = document;
	diagnostics.push({
		severity: DiagnosticSeverity.Error,
		range: {
			start: currDocument.positionAt(0),
			end: currDocument.positionAt(1)
		},
		message: "Ohh Looks like its api",
		source: 'Namma DSL',
	});
	return diagnostics;
}