Module MathsTokenizerAndParser
    Public Delegate Function OperatorDelegate(ByVal Param1 As Int32, ByVal Param2 As Int32) As Int32
    Private PrecedenceDictionary As New Dictionary(Of String, Int16) From {{"^", 4}, {"*", 3}, {"/", 3}, {"+", 2}, {"-", 2}, {"(", -1}, {")", -1}}
    Public Function GenerateTokens(ByVal input As String) As List(Of Token)
        Dim basePointer As Int32 = 0
        Dim topPointer As Int32 = basePointer
        Dim output As New List(Of Token)

        While True

            topPointer = basePointer

            While True
                Try
                    If PrecedenceDictionary.ContainsKey(input(basePointer)) Then
                        Exit While
                    End If
                    Convert.ToInt32(input.Substring(basePointer, topPointer - basePointer + 1))
                    topPointer += 1
                    Continue While
                Catch ex As Exception
                    output.Add(New Token(Convert.ToInt32(input.Substring(basePointer, topPointer - basePointer)), 0))
                    Exit While
                End Try
            End While

            basePointer = topPointer

            Try
                output.Add(New Token(input(basePointer), 2))
                basePointer += 1
            Catch ex As Exception
                Return output
            End Try

            While True
                If basePointer + 1 >= input.Count Then
                    Return output
                End If

                Try
                    Convert.ToInt32(input.Substring(basePointer, 1))
                    Exit While
                Catch ex As Exception
                    output.Add(New Token(input(basePointer), 2))
                    basePointer += 1
                End Try
            End While
        End While
    End Function
    Public Function EvaluatePostFix(ByVal Tokens As List(Of Token)) As Int32
        Dim numberStack As New Stack(Of Int32)
        Dim FunctionDictionary As New Dictionary(Of Char, [Delegate]) From {{"+", New OperatorDelegate(AddressOf Add)},
                                                                            {"-", New OperatorDelegate(AddressOf Subtract)},
                                                                            {"/", New OperatorDelegate(AddressOf Divide)},
                                                                            {"*", New OperatorDelegate(AddressOf Multiply)},
                                                                            {"^", New OperatorDelegate(AddressOf Exponent)}}
        While Tokens.Count > 0
            Dim NextToken As Token = Tokens(0)

            Tokens.RemoveAt(0)

            If NextToken.Type = 0 Then
                numberStack.Push(NextToken.Value)
            ElseIf NextToken.Type = 2 And NextToken.Value <> "(" And NextToken.Value <> ")" Then
                Dim First As Int32 = numberStack.Pop()
                Dim Second As Int32 = numberStack.Pop()

                numberStack.Push(FunctionDictionary(NextToken.Value).DynamicInvoke(Second, First))
            End If
        End While

        Return numberStack.Pop()
    End Function
    Private Function Subtract(ByVal Left As Int32, ByVal Right As Int32) As Int32
        Return Left - Right
    End Function
    Private Function Exponent(ByVal Left As Int32, ByVal Right As Int32) As Int32
        Return Left ^ Right
    End Function
    Private Function Divide(ByVal Top As Int32, ByVal Bottom As Int32) As Int32
        Return Top / Bottom
    End Function
    Private Function Multiply(ByVal Left As Int32, ByVal Right As Int32) As Int32
        Return Left * Right
    End Function
    Private Function Add(ByVal Left As Int32, ByVal Right As Int32) As Int32
        Return Left + Right
    End Function
    Public Function ShuntingYard(ByVal Input As List(Of Token)) As List(Of Token) ' Converts from infix notation to postfix https://en.wikipedia.org/wiki/Shunting-yard_algorithm Functions not yet tokenized
        Dim output As New List(Of Token)
        Dim OperatorStack As New Stack(Of Token)
        Dim nextToken As New Token()

        While Input.Count() > 0
            nextToken = Input(0)

            If nextToken.Type = 0 Or nextToken.Type = 1 Then
                output.Add(nextToken)
                Input.RemoveAt(0)
            ElseIf nextToken.Value = "(" Then
                OperatorStack.Push(nextToken)
                Input.RemoveAt(0)
            ElseIf nextToken.Type = 2 Then
                If OperatorStack.Count > 0 Then
                    While (OperatorStack.Count > 0 And (OperatorStack.Peek().Type = 1 Or OperatorStack.Peek().Precedence > nextToken.Precedence Or (OperatorStack.Peek().Precedence = nextToken.Precedence And OperatorStack.Peek().Value <> "^")) And OperatorStack.Peek().Value <> "(")
                        output.Add(OperatorStack.Pop())

                        If OperatorStack.Count = 0 Then
                            Exit While
                        End If
                    End While
                End If
                OperatorStack.Push(nextToken)
                Input.RemoveAt(0)

            ElseIf nextToken.Value = ")" Then
                While OperatorStack.Peek().Value <> "("
                    output.Add(OperatorStack.Pop())
                End While

                OperatorStack.Pop()
            End If

        End While

        While OperatorStack.Count() > 0
            If Not (OperatorStack.Peek().Value = "(" Or OperatorStack.Peek().Value = ")") Then
                output.Add(OperatorStack.Pop())
            Else
                OperatorStack.Pop()
            End If
        End While

        Return output
    End Function
    Public Class Token ' Types {<0, number> ; <1, Function> ; <2, Operator>}
        Public Value As String
        Public Type As Int16
        Public Precedence As Int16
        Public Sub New()
        End Sub
        Public Sub New(ByVal InValue As String, ByVal InType As Int16)
            Value = InValue
            Type = InType
            If PrecedenceDictionary.ContainsKey(InValue) Then
                Precedence = PrecedenceDictionary(InValue)
            Else
                Precedence = -1
            End If
        End Sub
    End Class
End Module

Module Module1 ' http://www.meta-calculator.com/learning-lab/how-to-build-scientific-calculator/
    Sub Main()
        Dim numberStack As Stack(Of Int32) = New Stack(Of Int32)
        Dim output As Int32 = EvaluatePostFix(ShuntingYard(GenerateTokens(Console.ReadLine())))

        Console.WriteLine(output)
        Console.ReadKey()
    End Sub

End Module