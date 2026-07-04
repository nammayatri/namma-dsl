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

import { validateApi } from './Validations/Api/validation';

import { validateStorage } from './Validations/Storage/validation';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

const yaml = require('yaml');
const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;
let currDocument:TextDocument;
let diagnostics: Diagnostic[] = [];
let parsedYAML: any;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	);
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			completionProvider: {
				resolveProvider: true
			}
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

interface NammaDSLSettings {
	maxNumberOfProblems: number;
}

const defaultSettings: NammaDSLSettings = { maxNumberOfProblems: 100, };

let globalSettings: NammaDSLSettings = defaultSettings;

const documentSettings: Map<string, Thenable<NammaDSLSettings>> = new Map();

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		documentSettings.clear();
	} else {
		globalSettings = <NammaDSLSettings>(
			(change.settings.nammaDSL || defaultSettings)
		);
	}
	documents.all().forEach(validateDSL);
});

documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
});

documents.onDidChangeContent(change => {
	diagnostics.length = 0;
	validateDSL(change.document);
	connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
});

function validateDSL(document: TextDocument) {
	currDocument = document;
	diagnostics = [];
	let isValidYaml = false;
	let content;
	try {
		content = document.getText();
		yaml.parse(content);
		parsedYAML = yaml.parseDocument(content,{lineCounter: true});
		isValidYaml = true;
	} catch (error:any) {
		diagnostics.push({
                severity: DiagnosticSeverity.Error,
                range: {
                    start: document.positionAt(error.pos[0]),
                    end: document.positionAt(error.pos[1])
                },
                message: error.message,
                source: 'Namma DSL',
            });
	}
	if (isValidYaml) {
		if (content?.startsWith('#!api'))
			diagnostics = diagnostics.concat(validateApi(parsedYAML, document));
		else
			diagnostics = diagnostics.concat(validateStorage(parsedYAML, document));
	}
}

connection.onDidChangeWatchedFiles(_change => {
	connection.console.log('Change occurred in watched files');
});

connection.onCompletion(
	(_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
		return [
			{
				label: 'Namma DSL',
				kind: CompletionItemKind.Text,
				data: 1
			},
			{
				label: 'Unknown',
				kind: CompletionItemKind.Text,
				data: 2
			}
		];
	}
);

connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		if (item.data === 1) {
			item.detail = 'Namma DSL details';
			item.documentation = 'Namma DSL documentation';
		} else if (item.data === 2) {
			item.detail = 'Unknown';
			item.documentation = 'Unknown details';
		}
		return item;
	}
);

documents.listen(connection);

connection.listen();
