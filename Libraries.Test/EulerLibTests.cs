using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using System.Linq;

namespace Libraries.Test
{
    [TestClass]
    public class EulerLibTests
    {
        [TestMethod()]
        public void IsPrimeTest()
        {
            Boolean resultPrimeTrue = EulerLib.IsPrime(7);
            Assert.IsTrue(resultPrimeTrue);

            Boolean resultPrimeFalse = EulerLib.IsPrime(6);
            Assert.IsFalse(resultPrimeFalse);
        }

        [TestMethod()]
        public void GetPrimesTest()
        {
            var result = EulerLib.GetPrimes(100);
            Assert.IsNotNull(result);
        }

        //[TestMethod()]
        //public void FindDistinctPrimesTest()
        //{
        //    var primes = EulerLib.GetPrimes(100).ToArray();
        //    var acc = EulerLib.CreateArrayOfInt(1);

        //    var result = EulerLib.FindDistinctPrimeFactors(100, 2, primes, acc);
        //    Assert.IsNotNull(result);
        //}

        [TestMethod()]
        public void FactorialTest()
        {
            Int32 result = EulerLib.Factorial(3, 1);
            Assert.AreEqual(result, 6);
        }

        [TestMethod()]
        public void FactorialBigIntTest()
        {
            System.Numerics.BigInteger result = EulerLib.FactorialBigInt(11);
            Assert.AreEqual(result, 39916800);
        }

        [TestMethod()]
        public void FibonacciArrayTest()
        {
            var result = EulerLib.FibonacciArray(1, 2, 99, FSharpList<int>.Empty);
            Assert.IsNotNull(result);
            Assert.AreEqual(result.Length, 8);
        }

        [TestMethod()]
        public void CollatzSequenceArrayTest()
        {
            var result = EulerLib.CollatzSequenceArray(13, 1, FSharpList<int>.Empty);
            Assert.IsNotNull(result);
            Assert.IsTrue(result.Length == 9);
        }

        [TestMethod()]
        public void CollatzSequenceCountTest()
        {
            int result = EulerLib.CollatzSequenceCount(13, 1);
            Assert.IsTrue(result == 10);
        }

        [TestMethod()]
        public void RandomArrayTest()
        {
            double[] result = EulerLib.RandomArray(30);
            Assert.IsTrue(result.Length == 30);
        }

    }
}
