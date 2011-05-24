/*
 * Licensed to the Sakai Foundation (SF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The SF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */

define([], function() {

        var Condition = function(){};

        //////////////////////////////////////////////////////////
        // Functions for manipulating boolean condition objects //
        //////////////////////////////////////////////////////////

		/**
         * Checks if the condition object can be converted from OR form to AND form.
         *
         * @return {boolean}true if the condition object can be safely converted from OR form to AND form; otherwise returns {boolean}false.
         */
        Condition.prototype.canConvertORtoAND = function() {
            if (!this.hasOwnProperty("OR")) return false;
            var len = this.OR.length;
            return len === 1 || len === 0;
        };

		/**
         * Checks if the condition object can be converted from AND form to OR form.
         *
         * @return {boolean}true if the condition object can be safely converted from AND form to OR form; otherwise returns {boolean}false.
         */
        Condition.prototype.canConvertANDtoOR = function() {
            if (!this.hasOwnProperty("AND")) return false;
            var len = this.AND.length;
            return len === 1 || len === 0;
        };

		/**
         * Converts the condition object from OR form to AND form.
         * The function doesn't do any checks before conversion.
         *
         */
        Condition.prototype.convertORtoAND = function() {
            this.AND = this.OR;
            delete this.OR;
        };

		/**
         * Converts the condition object from AND form to OR form.
         * The function doesn't do any checks before conversion.
         *
         */
        Condition.prototype.convertANDtoOR = function() {
            this.OR = this.AND;
            delete this.AND;
        };

		/**
		 * Checks if the condition object is empty, i.e. doesn't have AND or OR properties, or one of these properties contains an empty array
		 *
		 * @return {boolean}true if the condition object is empty; otherwise returns {boolean}false.
		 */
		Condition.prototype.isConditionObjectEmpty = function() {
			var objHasOwnPropertyAND = this.hasOwnProperty("AND");
			var objHasOwnPropertyOR = this.hasOwnProperty("OR");
			return (objHasOwnPropertyAND && this.AND.length === 0)
                    || (objHasOwnPropertyOR && this.OR.length === 0)
                    || (!objHasOwnPropertyAND && !objHasOwnPropertyOR);
		};

		/**
		 * Joins two condition objects by AND condition.
		 * This function tries to optimeze the output object to avoid excessive object wrapping.
		 * To avoid object cloning the function operates on its arguments, there is no guarantee that the arguments will remain unchanged.
		 *
		 * @param b	condition object to join (must contain either AND or OR field)
		 *
		 * @return a Condition object containing the this condition object joined by AND with the provided condition object.
		 */
		Condition.prototype.joinTwoConditionsByAND = function(b) {

            var isAEmpty = this.isConditionObjectEmpty();
			var isBEmpty = b.isConditionObjectEmpty();

			if(isAEmpty && isBEmpty) {
				return new Condition();
			}

			if(isAEmpty) {
				// trying to join empty object 'a' with 'b', just return 'b' in this case
				return b;
			}

			if(isBEmpty) {
				// trying to join empty object 'b' with 'a', just return 'a' in this case
				return this;
			}


			if(this.canConvertORtoAND()) {
				this.convertORtoAND();
			}

			if(b.canConvertORtoAND()){
				b.convertORtoAND();
			}

			var aHasOwnPropertyAND = this.hasOwnProperty("AND");
			var aHasOwnPropertyOR = this.hasOwnProperty("OR");

			var bHasOwnPropertyAND = b.hasOwnProperty("AND");
			var bHasOwnPropertyOR = b.hasOwnProperty("OR");

			if(aHasOwnPropertyAND && bHasOwnPropertyAND) {
				// simple array merge will do
				this.AND = this.AND.concat(b.AND);
				return this;
			}

			if(aHasOwnPropertyAND && bHasOwnPropertyOR) {
				// add b as array element to a.AND array
				this.AND.push(b);
				return this;
			}

			if(aHasOwnPropertyOR && bHasOwnPropertyAND) {
				// add a as array element to b.AND array
				b.AND.unshift(this);
				return b;
			}

			if(aHasOwnPropertyOR && bHasOwnPropertyOR) {
				// Need to wrap everything into a new object here
				var result = new Condition();
                result.AND = [this, b];
				return result;
			}

			return this; //default, we shouldn't be here
		};

		/**
		 * Joins two condition objects by OR condition.
		 * This function tries to optimeze the output object to avoid excessive object wrapping.
		 * To avoid object cloning the function operates on its arguments, there is no guarantee that the arguments will remain unchanged.
		 *
		 * @param b	condition object to join (must contain either AND or OR field)
		 *
		 * @return a Condition object containing the this condition object joined by OR with the provided condition object.
		 */
		Condition.prototype.joinTwoConditionsByOR = function(b) {

			var isAEmpty = this.isConditionObjectEmpty();
			var isBEmpty = b.isConditionObjectEmpty();

			if(isAEmpty && isBEmpty) {
				return new Condition();
			}

			if(isAEmpty) {
				// trying to join empty object 'a' with 'b', just return 'b' in this case
				return b;
			}

			if(isBEmpty) {
				// trying to join empty object 'b' with 'a', just return 'a' in this case
				return this;
			}


			if(this.canConvertANDtoOR()) {
				this.convertANDtoOR();
			}

			if(b.canConvertANDtoOR()){
				b.convertANDtoOR();
			}

			var aHasOwnPropertyAND = this.hasOwnProperty("AND");
			var aHasOwnPropertyOR = this.hasOwnProperty("OR");

			var bHasOwnPropertyAND = b.hasOwnProperty("AND");
			var bHasOwnPropertyOR = b.hasOwnProperty("OR");

			if(aHasOwnPropertyOR && bHasOwnPropertyOR) {
				// simple array merge will do
				this.OR = this.OR.concat(b.OR);
				return this;
			}

			if(aHasOwnPropertyOR && bHasOwnPropertyAND) {
				// add b as array element to a.OR array
				this.OR.push(b);
				return this;
			}

			if(aHasOwnPropertyAND && bHasOwnPropertyOR) {
				// add a as array element to b.OR array
				b.OR.unshift(this);
				return b;
			}

			if(aHasOwnPropertyAND && bHasOwnPropertyAND) {
				// Need to wrap everything into a new object here
				var result = new Condition();
                result.OR = [this, b];
				return result;
			}

			return this; //default, we shouldn't be here

		};



    return Condition;

});
