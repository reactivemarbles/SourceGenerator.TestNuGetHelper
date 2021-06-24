// Copyright (c) 2019-2021 ReactiveUI Association Incorporated. All rights reserved.
// ReactiveUI Association Incorporated licenses this file to you under the MIT license.
// See the LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

using ReactiveMarbles.ObservableEvents.Tests.Compilation;

namespace ReactiveMarbles.ObservableEvents.Tests
{
    /// <summary>
    /// The source generator utility which helps with getting NuGet packages and the source driver together.
    /// </summary>
    public class SourceGeneratorUtility
    {
        private static readonly MetadataReference[] SystemAssemblyReferences;

        private Action<string> _writeOutput;

        /// <summary>
        /// Initializes static members of the <see cref="SourceGeneratorUtility"/> class.
        /// </summary>
        static SourceGeneratorUtility()
        {
            var assemblies = new HashSet<MetadataReference>();

            foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
            {
                if (assembly.FullName is null)
                {
                    continue;
                }

                if (!assembly.FullName.StartsWith("System") && !assembly.FullName.StartsWith("mscordlib") && !assembly.FullName.StartsWith("netstandard"))
                {
                    continue;
                }

                if (assembly.IsDynamic)
                {
                    continue;
                }

                assemblies.Add(MetadataReference.CreateFromFile(assembly.Location));
            }

            SystemAssemblyReferences = assemblies.ToArray();
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SourceGeneratorUtility"/> class.
        /// </summary>
        /// <param name="writeOutput">Writes output for any errors found.</param>
        public SourceGeneratorUtility(Action<string> writeOutput)
        {
            _writeOutput = writeOutput ?? throw new ArgumentNullException(nameof(writeOutput));
        }

        /// <summary>
        /// Runs the generator.
        /// </summary>
        /// <typeparam name="T">The type of generator.</typeparam>
        /// <param name="compiler">The compiler.</param>
        /// <param name="compilationDiagnostics">The diagnostics which are produced from the compiler.</param>
        /// <param name="generatorDiagnostics">The diagnostics which are produced from the generator.</param>
        /// <param name="sources">The source code files.</param>
        /// <returns>The source generator instance.</returns>
        public T RunGenerator<T>(EventBuilderCompiler compiler, out ImmutableArray<Diagnostic> compilationDiagnostics, out ImmutableArray<Diagnostic> generatorDiagnostics, params string[] sources)
            where T : ISourceGenerator, new()
        {
            var compilation = CreateCompilation(compiler, sources);

            var generator = new T();

            var newCompilation = RunGenerators(compilation, out generatorDiagnostics, new T());

            compilationDiagnostics = newCompilation.GetDiagnostics();

            ShouldHaveNoCompilerDiagnosticsWarningOrAbove(_writeOutput, newCompilation, compilationDiagnostics);
            ShouldHaveNoCompilerDiagnosticsWarningOrAbove(_writeOutput, compilation, generatorDiagnostics);

            return generator;
        }

        private static void ShouldHaveNoCompilerDiagnosticsWarningOrAbove(Action<string> output, Microsoft.CodeAnalysis.Compilation compilation, IEnumerable<Diagnostic> diagnostics)
        {
            var compilationErrors = diagnostics.Where(x => x.Severity >= DiagnosticSeverity.Warning).Select(x => $"// {x.Location.SourceTree?.FilePath} ({x.Location.GetLineSpan().StartLinePosition}): {x.GetMessage()}{Environment.NewLine}").ToList();

            var outputSources = string.Join(Environment.NewLine, compilation.SyntaxTrees.Select(x => $"// {x.FilePath}:{Environment.NewLine}{x}").Where(x => !x.Contains("The impementation should have been generated.")));

            if (compilationErrors.Count > 0)
            {
                output?.Invoke(outputSources);
                throw new InvalidOperationException(string.Join(Environment.NewLine, compilationErrors));
            }
        }

        private static Microsoft.CodeAnalysis.Compilation CreateCompilation(EventBuilderCompiler compiler, params string[] sources)
        {
            var assemblyPath = Path.GetDirectoryName(typeof(object).Assembly.Location);

            if (assemblyPath == null || string.IsNullOrWhiteSpace(assemblyPath))
            {
                throw new InvalidOperationException("Could not find a valid assembly path.");
            }

            var assemblies = new HashSet<MetadataReference>();

            assemblies.UnionWith(compiler.Modules.Where(x => x.PEFile is not null).Select(x => MetadataReference.CreateFromFile(x.PEFile!.FileName)));
            assemblies.UnionWith(compiler.ReferencedModules.Where(x => x.PEFile is not null).Select(x => MetadataReference.CreateFromFile(x.PEFile!.FileName)));
            assemblies.UnionWith(compiler.NeededModules.Where(x => x.PEFile is not null).Select(x => MetadataReference.CreateFromFile(x.PEFile!.FileName)));

            return CSharpCompilation.Create(
                assemblyName: "compilation" + Guid.NewGuid(),
                syntaxTrees: sources.Select(x => CSharpSyntaxTree.ParseText(x, new CSharpParseOptions(LanguageVersion.Latest))),
                references: assemblies,
                options: new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, deterministic: true));
        }

        /// <summary>
        /// Executes the source generators.
        /// </summary>
        /// <param name="compilation">The target compilation.</param>
        /// <param name="diagnostics">The resulting diagnostics.</param>
        /// <param name="generators">The generators to include in the compilation.</param>
        /// <returns>The new compilation after the generators have executed.</returns>
        private static Microsoft.CodeAnalysis.Compilation RunGenerators(Microsoft.CodeAnalysis.Compilation compilation, out ImmutableArray<Diagnostic> diagnostics, params ISourceGenerator[] generators)
        {
            CreateDriver(compilation, generators).RunGeneratorsAndUpdateCompilation(compilation, out var outputCompilation, out diagnostics);
            return outputCompilation;
        }

        private static GeneratorDriver CreateDriver(Microsoft.CodeAnalysis.Compilation compilation, params ISourceGenerator[] generators) =>
            CSharpGeneratorDriver.Create(
                generators: ImmutableArray.Create(generators),
                additionalTexts: ImmutableArray<AdditionalText>.Empty,
                parseOptions: (CSharpParseOptions)compilation.SyntaxTrees.First().Options,
                optionsProvider: null);

        private sealed class AssemblyEqualityComparer : IEqualityComparer<Assembly>
        {
            private AssemblyEqualityComparer()
            {
            }

            public static AssemblyEqualityComparer Default { get; } = new AssemblyEqualityComparer();

            public bool Equals(Assembly? x, Assembly? y)
            {
                if (x is null && y is null)
                {
                    return true;
                }

                if (x is null || y is null)
                {
                    return false;
                }

                return string.Equals(x.FullName, y.FullName, StringComparison.InvariantCultureIgnoreCase);
            }

            public int GetHashCode(Assembly obj) => obj.FullName?.GetHashCode() ?? 0;
        }
    }
}
